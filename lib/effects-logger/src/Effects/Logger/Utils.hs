{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal utilities.
--
-- @since 0.1
module Effects.Logger.Utils
  ( -- * Namespace
    Namespace (..),

    -- ** Logging functions

    -- * Formatting
    LogFormatter (..),
    defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    logStrToBs,
    logStrToText,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Logger
  ( Loc (Loc),
    LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    ToLogStr (toLogStr),
  )
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable (Foldable (foldMap'))
#if MIN_VERSION_base(4, 18, 0)
import Data.Functor ((<&>))
#endif
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Concurrent.Thread (MonadThread)
import Effects.Concurrent.Thread qualified as Thread
import Effects.Time (MonadTime (getSystemZonedTime), getSystemTime)
import Effects.Time qualified as MonadTime
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import Optics.Core
  ( A_Lens,
    An_Iso,
    LabelOptic (labelOptic),
    iso,
    lensVL,
    view,
    (^.),
  )
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
instance
  (k ~ An_Iso, a ~ Seq Text, b ~ Seq Text) =>
  LabelOptic "unNamespace" k Namespace Namespace a b
  where
  labelOptic = iso (\(MkNamespace ns) -> ns) MkNamespace
  {-# INLINE labelOptic #-}

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

-- | Formatter for logs.
--
-- @since 0.1
data LogFormatter = MkLogFormatter
  { -- | How to log the code location.
    --
    -- @since 0.1
    locStrategy :: !LocStrategy,
    -- | If true, append a newline.
    --
    -- @since 0.1
    newline :: !Bool,
    -- | Whether to include the thread's label set by 'Thread.labelThread'.
    -- Falls back to the thread's 'Thread.ThreadId' when the label has not been set.
    --
    -- @since 0.1
    threadLabel :: !Bool,
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
instance
  (k ~ A_Lens, a ~ LocStrategy, b ~ LocStrategy) =>
  LabelOptic "locStrategy" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
      fmap
        (\b -> MkLogFormatter b a2 a3 a4)
        (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "newline" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
      fmap
        (\b -> MkLogFormatter a1 b a3 a4)
        (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "threadLabel" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
      fmap
        (\b -> MkLogFormatter a1 a2 b a4)
        (f a3)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "timezone" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter a1 a2 a3 a4) ->
      fmap
        (\b -> MkLogFormatter a1 a2 a3 b)
        (f a4)
  {-# INLINE labelOptic #-}

-- | 'LogFormatter' with:
--
-- @
-- locStrategy = 'LocPartial' loc
-- newline = 'True'
-- threadLabel = 'False'
-- timezone = 'False'
-- @
--
-- @since 0.1
defaultLogFormatter :: Loc -> LogFormatter
defaultLogFormatter loc =
  MkLogFormatter
    { locStrategy = LocPartial loc,
      newline = True,
      threadLabel = False,
      timezone = False
    }

-- | Produces a formatted 'LogStr'.
--
-- __Example__
--
-- @
-- -- [timestamp][thread_label][namespace][code_loc][level] msg
-- [2022-02-08 10:20:05][thread-label][one.two][filename:1:2][Warn] msg
-- @
--
-- @since 0.1
formatLog ::
  ( HasCallStack,
    MonadThread m,
    MonadTime m,
    ToLogStr msg
  ) =>
  -- | Possible namespace.
  Maybe Namespace ->
  -- | Formatter to use.
  LogFormatter ->
  -- | The level in which to log.
  LogLevel ->
  -- | Message.
  msg ->
  -- | Formatted LogStr.
  m LogStr
formatLog mNamespace formatter lvl msg = do
  timestampTxt <- timeFn
  let namespaceTxt = case mNamespace of
        Nothing -> ""
        Just namespace -> brackets $ toLogStr $ displayNamespace namespace
  threadLabel <-
    if formatter ^. #threadLabel
      then getThreadLabel
      else pure ""
  let locTxt = case formatter ^. #locStrategy of
        LocPartial loc -> (brackets . toLogStr . partialLoc) loc
        LocStable loc -> (brackets . toLogStr . stableLoc) loc
        LocNone -> ""
      lvlTxt = toLogStr $ showLevel lvl
      msgTxt = toLogStr msg
      newline'
        | formatter ^. #newline = "\n"
        | otherwise = ""
      formatted =
        mconcat
          [ brackets timestampTxt,
            threadLabel,
            namespaceTxt,
            locTxt,
            brackets lvlTxt,
            " ",
            msgTxt,
            newline'
          ]
  pure formatted
  where
    timeFn =
      if formatter ^. #timezone
        then toLogStr . MonadTime.formatZonedTime <$> getSystemZonedTime
        else toLogStr . MonadTime.formatLocalTime <$> getSystemTime
{-# INLINEABLE formatLog #-}

{- ORMOLU_DISABLE -}

-- | Retrieves the thread label or thread id, if the former has not been set.
getThreadLabel :: (HasCallStack, MonadThread m) => m LogStr
getThreadLabel = do
#if MIN_VERSION_base(4, 18, 0)
  tid <- Thread.myThreadId
  Thread.threadLabel tid <&> \case
    Just label -> bracketsLogStr label
    Nothing -> bracketsLogStr $ show tid
#else
  bracketsLogStr . show <$> Thread.myThreadId
#endif
  where
    bracketsLogStr = brackets . toLogStr
{-# INLINEABLE getThreadLabel #-}

{- ORMOLU_ENABLE -}

partialLoc :: Loc -> Builder
partialLoc Loc {loc_filename, loc_start} =
  mconcat
    [ fromString loc_filename,
      ":" <> line,
      ":" <> char
    ]
  where
    (locLine, locChar) = loc_start
    line = fromString $ show locLine
    char = fromString $ show locChar

stableLoc :: Loc -> Builder
stableLoc = fromString . view #loc_filename

showLevel :: LogLevel -> Text
showLevel LevelDebug = "Debug"
showLevel LevelInfo = "Info"
showLevel LevelWarn = "Warn"
showLevel LevelError = "Error"
showLevel (LevelOther txt) = txt

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
