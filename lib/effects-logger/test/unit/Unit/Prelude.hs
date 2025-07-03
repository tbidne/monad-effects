{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Prelude
  ( -- * Type
    Logger (..),
    Env (..),
    runLogger,

    -- * Formatters
    format,
    formatNamespaced,
    formatTrace,
    formatTraceNamespaced,
    formatFatal,
    formatFatalNamespaced,
  )
where

import Control.Concurrent qualified as CC
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
#if MIN_VERSION_base(4, 18, 0)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
#else
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
#endif
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Concurrent.Thread qualified as Thread
import Effects.Logger
  ( LogFormatter,
    LogLevel (LevelOther, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
  )
import Effects.Logger qualified as Logger
import Effects.Logger.Namespace
  ( Namespace,
    addNamespace,
  )
import Effects.Logger.Namespace qualified as Namespace
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
  )
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    LabelOptic',
    Prism',
    lens,
    prism,
  )

data Env = MkEnv
  { namespace :: Namespace,
    threadLabel :: Maybe String
  }

instance
  (k ~ A_Lens, a ~ Namespace, b ~ Namespace) =>
  LabelOptic "namespace" k Env Env a b
  where
  labelOptic =
    lens
      (\(MkEnv l _) -> l)
      (\(MkEnv _ n) l -> MkEnv l n)

newtype Logger a = MkLogger (ReaderT Env IO a)
  deriving (Applicative, Functor, Monad) via (ReaderT Env IO)
  deriving (MonadReader Env) via (ReaderT Env IO)

runLogger :: Logger a -> Namespace -> IO a
runLogger (MkLogger rdr) ns = runReaderT rdr (MkEnv ns Nothing)

instance MonadThread Logger where
  myThreadId = MkLogger $ liftIO CC.myThreadId

#if MIN_VERSION_base(4, 18, 0)
  threadLabel _ = MkLogger $ asks (.threadLabel)
#endif

instance MonadTime Logger where
  getSystemZonedTime = pure zonedTime
  getMonotonicTime = pure 50

instance Effects.Logger.MonadLogger Logger where
  monadLoggerLog _loc _src _lvl _msg = pure ()

format ::
  forall m env k.
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
format = format' Logger.formatLog

formatNamespaced ::
  forall m env k.
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatNamespaced = format' Namespace.formatLog

format' ::
  forall m env k.
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  ) =>
  (LogFormatter -> LogLevel -> Text -> m LogStr) ->
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
format' logFn fmt =
  addNamespace "one" $
    addNamespace "two" $
      logFn fmt Effects.Logger.LevelWarn "msg"

formatTrace ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatTrace = formatTrace' Logger.formatLog

formatTraceNamespaced ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatTraceNamespaced = formatTrace' Namespace.formatLog

formatTrace' ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  ) =>
  (LogFormatter -> LogLevel -> Text -> m LogStr) ->
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatTrace' logFn fmt =
  addNamespace "one" $
    addNamespace "two" $
      logFn fmt (Effects.Logger.LevelOther "Trace") "msg"

formatFatal ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatFatal = formatFatal' Logger.formatLog

formatFatalNamespaced ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatFatalNamespaced = formatFatal' Namespace.formatLog

formatFatal' ::
  ( k ~ A_Lens,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  ) =>
  (LogFormatter -> LogLevel -> Text -> m LogStr) ->
  Effects.Logger.LogFormatter ->
  m Effects.Logger.LogStr
formatFatal' logFn fmt =
  addNamespace "one" $
    addNamespace "two" $
      logFn fmt (Effects.Logger.LevelOther "Fatal") "msg"

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

-- This is not lawful but it is useful here, so w/e
_LevelCustom :: Prism' Effects.Logger.LogLevel Text
_LevelCustom =
  prism
    Effects.Logger.LevelOther
    ( \case
        x@(Effects.Logger.LevelOther "Trace") -> Left x
        x@(Effects.Logger.LevelOther "Fatal") -> Left x
        Effects.Logger.LevelOther c -> Right c
        other -> Left other
    )
