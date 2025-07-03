{-# LANGUAGE TemplateHaskell #-}

-- | Provides logging effect and utilities..
--
-- @since 0.1
module Effects.Logger
  ( -- * Effect
    MonadLogger (..),

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
    Utils.defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * Optics

    -- ** LogLevels
    _LevelTrace,
    _LevelInfo,
    _LevelDebug,
    _LevelWarn,
    _LevelError,
    _LevelOther,
    _LevelFatal,

    -- * Reexports
    LogStr,
    Loc,
  )
where

import Control.Monad (when)
import Control.Monad.Logger
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
    ToLogStr,
    liftLoc,
  )
import Control.Monad.Logger qualified as MLogger
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Logger.Utils
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
  )
import Effects.Logger.Utils qualified as Utils
import Effects.Time (MonadTime)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc)
import Language.Haskell.TH.Syntax (Exp, Lift (lift), Q, Quasi (qLocation))
import Optics.Core
  ( Prism',
    preview,
    prism,
  )

-- | Produces a formatted 'LogStr'.
--
-- __Example__
--
-- @
-- -- [timestamp][thread_label][code_loc][level] msg
-- [2022-02-08 10:20:05][thread-label][filename:1:2][Warn] msg
-- @
--
-- @since 0.1
formatLog ::
  ( HasCallStack,
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
formatLog = Utils.formatLog Nothing
{-# INLINEABLE formatLog #-}

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
-- In other words, 'LogLevel'\'s usual 'Ord' is respected, with the additional
-- cases. Note that any other @LevelOther "custom"@ sit at the the highest
-- level and compare via Text's 'Ord', just like 'LogLevel'\'s usual 'Ord'.
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
