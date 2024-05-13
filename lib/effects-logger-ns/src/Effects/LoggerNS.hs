{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides namespaced logging functionality on top of 'MonadLogger'.
--
-- @since 0.1
module Effects.LoggerNS
  ( -- * Effect
    MonadLoggerNS (..),
    Namespace (..),
    addNamespace,

    -- * Levels
    LogLevel (..),
    levelTrace,
    levelFatal,

    -- ** Logging functions

    -- *** Levels
    logTrace,
    MLogger.logDebug,
    MLogger.logInfo,
    MLogger.logWarn,
    MLogger.logError,
    MLogger.logOther,
    logFatal,

    -- *** Level checks
    guardLevel,
    shouldLog,

    -- * Formatting
    LogFormatter (..),
    defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    logStrToBs,
    logStrToText,

    -- * Optics

    -- ** LogLevels
    _LevelTrace,
    _LevelInfo,
    _LevelDebug,
    _LevelWarn,
    _LevelError,
    _LevelOther,
    _LevelFatal,

    -- ** LocStrategy
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
import Control.Monad (when)
import Control.Monad.Logger
  ( Loc (Loc),
    LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
    ToLogStr (toLogStr),
    liftLoc,
  )
import Control.Monad.Logger qualified as MLogger
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Foldable (Foldable (foldMap'))
#if MIN_VERSION_base(4, 18, 0)
import Data.Functor ((<&>))
#endif
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error qualified as TEncError
import Data.Word (Word8)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Concurrent.Thread qualified as Thread
import Effects.Time (MonadTime (getSystemZonedTime), getSystemTime)
import Effects.Time qualified as MonadTime
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc (loc_filename, loc_start))
import Language.Haskell.TH.Syntax (Exp, Lift (lift), Q, Quasi (qLocation))
import Optics.Core
  ( A_Lens,
    An_Iso,
    LabelOptic (labelOptic),
    Prism',
    iso,
    lensVL,
    over',
    preview,
    prism,
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

-- | Adds namespaces to 'MonadLogger'.
--
-- @since 0.1
class (MonadLogger m) => MonadLoggerNS m where
  -- | Retrieves the namespace.
  --
  -- @since 0.1
  getNamespace :: (HasCallStack) => m Namespace

  -- | Locally modifies the namespace.
  --
  -- @since 0.1
  localNamespace :: (HasCallStack) => (Namespace -> Namespace) -> m a -> m a

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  ( MonadLoggerNS m
  ) =>
  Text ->
  m a ->
  m a
addNamespace txt = localNamespace (over' #unNamespace (:|> txt))
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
_LocPartial :: Prism' LocStrategy Loc
_LocPartial =
  prism
    LocPartial
    ( \case
        LocPartial loc -> Right loc
        other -> Left other
    )
{-# INLINE _LocPartial #-}

-- | @since 0.1
_LocStable :: Prism' LocStrategy Loc
_LocStable =
  prism
    LocStable
    ( \case
        LocStable loc -> Right loc
        other -> Left other
    )
{-# INLINE _LocStable #-}

-- | @since 0.1
_LocNone :: Prism' LocStrategy ()
_LocNone =
  prism
    (const LocNone)
    ( \case
        LocNone -> Right ()
        other -> Left other
    )
{-# INLINE _LocNone #-}

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
    -- Falls back to the thread's 'ThreadId' when the label has not been set.
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
    lensVL $ \f (MkLogFormatter _locStrategy _newline _threadLabel _timezone) ->
      fmap
        (\locStrategy' -> MkLogFormatter locStrategy' _newline _threadLabel _timezone)
        (f _locStrategy)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "newline" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter _locStrategy _newline _threadLabel _timezone) ->
      fmap
        (\newline' -> MkLogFormatter _locStrategy newline' _threadLabel _timezone)
        (f _newline)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "threadLabel" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter _locStrategy _newline _threadLabel _timezone) ->
      fmap
        (\threadLabel' -> MkLogFormatter _locStrategy _newline threadLabel' _timezone)
        (f _threadLabel)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "timezone" k LogFormatter LogFormatter a b
  where
  labelOptic =
    lensVL $ \f (MkLogFormatter _locStrategy _newline _threadLabel _timezone) ->
      fmap
        (MkLogFormatter _locStrategy _newline _threadLabel)
        (f _timezone)
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
-- @since 0.1
formatLog ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadThread m,
    MonadTime m,
    ToLogStr msg
  ) =>
  -- | Formatter to use.
  LogFormatter ->
  -- | The level in which to log.
  LogLevel ->
  -- | Message.
  msg ->
  -- | Formatted LogStr.
  m LogStr
formatLog formatter lvl msg = do
  timestampTxt <- timeFn
  namespace <- getNamespace
  threadLabel <-
    if formatter ^. #threadLabel
      then getThreadLabel
      else pure ""
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
            threadLabel,
            brackets namespaceTxt,
            brackets lvlTxt,
            locTxt,
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

-- Vendored from monad-logger
logTH :: LogLevel -> Q Exp
logTH level =
  [|
    monadLoggerLog $(qLocation >>= liftLoc) (T.pack "") $(lift level)
      . (id :: Text -> Text)
    |]

-- | @since 0.1
levelTrace :: LogLevel
levelTrace = LevelOther "Trace"

-- | @since 0.1
levelFatal :: LogLevel
levelFatal = LevelOther "Fatal"

-- | @since 0.1
logTrace :: Q Exp
logTrace = logTH levelTrace

-- | @since 0.1
logFatal :: Q Exp
logFatal = logTH levelFatal

-- | @guardLevel configLvl lvl m@ runs @m@ iff @'shouldLog' configLvl lvl@.
-- This can be useful for writing a logging function e.g.
--
-- @
--   -- logs msg to file iff configLogLevel <= lvl e.g.
--   -- configLogLevel := 'LevelWarn'
--   -- lvl            := 'LevelError'
--   logMsg lvl msg = do
--   configLogLevel <- getConfigLogLevel -- e.g. ReaderT Env
--   guardLevel configLogLevel lvl $ do
--     logToFile msg
-- @
--
-- @since 0.1
guardLevel ::
  (Applicative f) =>
  -- | The configured log level to check against.
  LogLevel ->
  -- | The log level for this action.
  LogLevel ->
  -- | The logging action to run if the level passes.
  f () ->
  f ()
guardLevel configLvl lvl = when (shouldLog configLvl lvl)
{-# INLINEABLE guardLevel #-}

-- | @shouldLog configLvl lvl@ returns true iff @configLvl <= lvl@. Uses
-- LogLevel's built-in ordering with special cases for "Trace"
-- (@LevelOther "Trace"@) and "Fatal" (@LevelOther "Fatal"@). The ad-hoc
-- ordering is thus:
--
-- @
--   LevelOther \"Trace\"
--     < LevelDebug
--     < LevelInfo
--     < LevelWarn
--     < LevelError
--     < LevelOther \"Fatal\"
--     < LevelOther \"\<any\>\"
-- @
--
-- In other words, 'LogLevel''s usual 'Ord' is respected, with the additional
-- cases. Note that any other @LevelOther "custom"@ sit at the the highest
-- level and compare via Text's 'Ord', just like 'LogLevel''s usual 'Ord'.
--
-- @since 0.1
shouldLog ::
  -- | The configured log level to check against.
  LogLevel ->
  -- | Level for this log
  LogLevel ->
  -- | Whether we should log
  Bool
shouldLog configLvl lvl =
  -- If both are LevelOther and not Trace/Fatal then we need to compare
  -- labels, as that is how Ord works.
  case (preview _LevelOther configLvl, preview _LevelOther lvl) of
    (Just configTxt, Just lvlTxt)
      | isCustom configTxt && isCustom lvlTxt -> configTxt <= lvlTxt
    _ -> logLevelToWord configLvl <= logLevelToWord lvl
  where
    isCustom t =
      t /= "Trace" && t /= "Fatal"

-- | @since 0.1
_LevelTrace :: Prism' LogLevel ()
_LevelTrace =
  prism
    (const levelTrace)
    ( \case
        LevelOther "Trace" -> Right ()
        other -> Left other
    )
{-# INLINE _LevelTrace #-}

-- | @since 0.1
_LevelDebug :: Prism' LogLevel ()
_LevelDebug =
  prism
    (const LevelDebug)
    ( \case
        LevelDebug -> Right ()
        other -> Left other
    )
{-# INLINE _LevelDebug #-}

-- | @since 0.1
_LevelInfo :: Prism' LogLevel ()
_LevelInfo =
  prism
    (const LevelInfo)
    ( \case
        LevelInfo -> Right ()
        other -> Left other
    )
{-# INLINE _LevelInfo #-}

-- | @since 0.1
_LevelWarn :: Prism' LogLevel ()
_LevelWarn =
  prism
    (const LevelWarn)
    ( \case
        LevelWarn -> Right ()
        other -> Left other
    )
{-# INLINE _LevelWarn #-}

-- | @since 0.1
_LevelError :: Prism' LogLevel ()
_LevelError =
  prism
    (const LevelError)
    ( \case
        LevelError -> Right ()
        other -> Left other
    )
{-# INLINE _LevelError #-}

-- | @since 0.1
_LevelOther :: Prism' LogLevel Text
_LevelOther =
  prism
    LevelOther
    ( \case
        LevelOther l -> Right l
        other -> Left other
    )
{-# INLINE _LevelOther #-}

-- | @since 0.1
_LevelFatal :: Prism' LogLevel ()
_LevelFatal =
  prism
    (const levelFatal)
    ( \case
        LevelOther "Fatal" -> Right ()
        other -> Left other
    )
{-# INLINE _LevelFatal #-}

logLevelToWord :: LogLevel -> Word8
logLevelToWord (LevelOther "Trace") = 0
logLevelToWord LevelDebug = 1
logLevelToWord LevelInfo = 2
logLevelToWord LevelWarn = 3
logLevelToWord LevelError = 4
logLevelToWord (LevelOther "Fatal") = 5
logLevelToWord (LevelOther _) = 6
