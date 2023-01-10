{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides namespaced logging functionality on top of 'MonadLogger'.
--
-- @since 0.1
module Effects.MonadLoggerNamespace
  ( -- * Class
    MonadLoggerNamespace (..),
    Namespace (..),
    addNamespace,

    -- * Formatting
    LogFormatter (..),
    defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    logStrToBs,
    logStrToText,

    -- * Optics
    _LocPartial,
    _LocStable,
    _LocNone,

    -- * Reexports
    MonadLogger (..),
    LogStr,
    Loc,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
    ToLogStr (toLogStr),
  )
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable (Foldable (foldMap'))
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Effects.MonadTime (MonadTime (getSystemTime, getSystemZonedTime))
import Effects.MonadTime qualified as MonadTime
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import Optics.Core (over', view, (%), (^.), _1, _2)
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)
import System.Log.FastLogger qualified as FL

-- | Logging namespace.
--
-- @since 0.1
newtype Namespace = MkNamespace
  { -- | @since 0.1
    unNamespace :: Seq Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      Semigroup
    )
    via (Seq Text)
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''Namespace

-- | @since 0.1
instance IsString Namespace where
  fromString = MkNamespace . Seq.singleton . T.pack
  {-# INLINEABLE fromString #-}

-- | @since 0.1
instance IsList Namespace where
  type Item Namespace = Text
  fromList = MkNamespace . fromList
  {-# INLINEABLE fromList #-}
  toList = toList . unNamespace
  {-# INLINEABLE toList #-}

displayNamespace :: Namespace -> Text
displayNamespace =
  foldMap' id
    . Seq.intersperse "."
    . view #unNamespace

-- | Adds namespaces to 'MonadLogger'.
--
-- @since 0.1
class MonadLogger m => MonadLoggerNamespace m where
  -- | Retrieves the namespace.
  --
  -- @since 0.1
  getNamespace :: HasCallStack => m Namespace

  -- | Locally modifies the namespace.
  --
  -- @since 0.1
  localNamespace :: HasCallStack => (Namespace -> Namespace) -> m a -> m a

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  ( MonadLoggerNamespace m
  ) =>
  Text ->
  m a ->
  m a
addNamespace txt = localNamespace (over' #unNamespace (|> txt))
{-# INLINEABLE addNamespace #-}

-- | Determines how we log location data.
--
-- @since 0.1
data LocStrategy
  = -- | Logs the location with filename, line, col.
    --
    -- @since 0.1
    LocPartial !Loc
  | -- | Logs the location with filename.
    --
    -- @since 0.1
    LocStable !Loc
  | -- | No location logging.
    --
    -- @since 0.1
    LocNone
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''LocStrategy

-- | Formatter for logs.
--
-- @since 0.1
data LogFormatter = MkLogFormatter
  { -- | If true, append a newline.
    --
    -- @since 0.1
    newline :: !Bool,
    -- | How to log the code location.
    --
    -- @since 0.1
    locStrategy :: !LocStrategy,
    -- | Whether to include the timezone in the timestamp.
    --
    -- @since 0.1
    timezone :: !Bool
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''LogFormatter

-- | 'LogFormatter' with:
--
-- @
-- 'newline' = 'True'
-- 'locStrategy' = 'LocPartial' loc
-- 'timezone' = 'False'
-- @
--
-- @since 0.1
defaultLogFormatter :: Loc -> LogFormatter
defaultLogFormatter loc =
  MkLogFormatter
    { newline = True,
      locStrategy = LocPartial loc,
      timezone = False
    }

-- | Produces a formatted 'LogStr'.
--
-- @since 0.1
formatLog ::
  ( HasCallStack,
    MonadLoggerNamespace m,
    MonadTime m,
    ToLogStr msg
  ) =>
  LogFormatter ->
  LogLevel ->
  msg ->
  m LogStr
formatLog formatter lvl msg = do
  timestampTxt <- timeFn
  namespace <- getNamespace
  let locTxt = case formatter ^. #locStrategy of
        LocPartial loc -> (brackets . toLogStr . partialLoc) loc
        LocStable loc -> (brackets . toLogStr . stableLoc) loc
        LocNone -> ""
      namespaceTxt = toLogStr $ displayNamespace namespace
      lvlTxt = toLogStr $ showLevel lvl
      msgTxt = toLogStr msg
      newline'
        | formatter ^. #newline = "\n"
        | otherwise = ""
      formatted =
        mconcat
          [ brackets timestampTxt,
            brackets namespaceTxt,
            brackets lvlTxt,
            locTxt,
            " ",
            msgTxt,
            newline'
          ]
  pure formatted
  where
    timeFn
      | formatter ^. #timezone =
          toLogStr . MonadTime.formatZonedTime <$> getSystemZonedTime
      | otherwise =
          toLogStr . MonadTime.formatLocalTime <$> getSystemTime
{-# INLINEABLE formatLog #-}

partialLoc :: Loc -> Builder
partialLoc loc =
  mconcat
    [ fromString $ view #loc_filename loc,
      ":" <> mkLine loc,
      ":" <> mkChar loc
    ]
  where
    mkLine = fromString . show . view (#loc_start % _1)
    mkChar = fromString . show . view (#loc_start % _2)

stableLoc :: Loc -> Builder
stableLoc loc = fromString $ view #loc_filename loc

showLevel :: LogLevel -> Text
showLevel LevelDebug = "Debug"
showLevel LevelInfo = "Info"
showLevel LevelWarn = "Warn"
showLevel LevelError = "Error"
showLevel (LevelOther txt) = "Other " <> txt

-- LogStr uses ByteString's Builder internally, so we might as well use it
-- for constants.
brackets :: LogStr -> LogStr
brackets m = cLogStr "[" <> m <> cLogStr "]"

cLogStr :: Builder -> LogStr
cLogStr = toLogStr @Builder

-- | @since 0.1
logStrToBs :: LogStr -> ByteString
logStrToBs = FL.fromLogStr

-- | @since 0.1
logStrToText :: LogStr -> Text
logStrToText = TEnc.decodeUtf8With TEncError.lenientDecode . FL.fromLogStr
