{-# LANGUAGE CPP #-}

-- | Provides an effect for desktop notifications.
--
-- @since 0.1
module Effects.Notify
  ( -- * Description
    -- $desc

    -- * Effect
    MonadNotify (..),

    -- ** Errors
    NotifyInitException (..),
    NotifyException (..),
    catchNonFatalNotify,
    tryNonFatalNotify,
    tryNonFatalNotify_,

    -- * Types

    -- ** System

    -- *** Platform-specific
    NotifySystem.NotifySystemOs (..),
    defaultNotifySystemOs,

    -- *** All platforms
    NotifySystem (..),
    defaultNotifySystem,
    notifySystemToOs,
    notifySystemFromOs,
    Os (..),
    NotifyParseException (..),

    -- ** Env
    NotifyEnv,
    notifyEnvToSystemOs,

    -- ** Notes
    Note,
    mkNote,
    setBody,
    getBody,
    setSummary,
    getSummary,
    setTimeout,
    getTimeout,
    setTitle,
    getTitle,
    setUrgency,
    getUrgency,

    -- ** Timeout
    NotifyTimeout (..),
    _NotifyTimeoutMillis,
    _NotifyTimeoutNever,

    -- ** NotifyUrgency
    NotifyUrgency (..),
    _NotifyUrgencyLow,
    _NotifyUrgencyNormal,
    _NotifyUrgencyCritical,
  )
where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch qualified as C
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Kind (Type)
import Effects.Notify.Internal.Data.Note
  ( Note,
    getBody,
    getSummary,
    getTimeout,
    getTitle,
    getUrgency,
    mkNote,
    setBody,
    setSummary,
    setTimeout,
    setTitle,
    setUrgency,
  )
import Effects.Notify.Internal.Data.NotifyEnv (NotifyEnv, notifyEnvToSystemOs)
import Effects.Notify.Internal.Data.NotifyException
  ( NotifyException (MkNotifyException, exception, fatal, note, notifySystem),
  )
import Effects.Notify.Internal.Data.NotifyInitException
  ( NotifyInitException (MkNotifyInitException, unNotifyInitException),
  )
import Effects.Notify.Internal.Data.NotifySystem
  ( NotifyParseException (MkNotifyParseException, os, system),
    NotifySystem
      ( NotifySystemAppleScript,
        NotifySystemDBus,
        NotifySystemNotifySend,
        NotifySystemWindows
      ),
    NotifySystemOs,
    defaultNotifySystem,
    defaultNotifySystemOs,
    notifySystemFromOs,
    notifySystemToOs,
  )
import Effects.Notify.Internal.Data.NotifySystem qualified as NotifySystem
import Effects.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (NotifyTimeoutMillis, NotifyTimeoutNever),
    _NotifyTimeoutMillis,
    _NotifyTimeoutNever,
  )
import Effects.Notify.Internal.Data.NotifyUrgency
  ( NotifyUrgency (NotifyUrgencyCritical, NotifyUrgencyLow, NotifyUrgencyNormal),
    _NotifyUrgencyCritical,
    _NotifyUrgencyLow,
    _NotifyUrgencyNormal,
  )
import Effects.Notify.Internal.Os (Os (Linux, Osx, Windows))
#if LINUX
import Effects.Notify.Internal.Os.Linux qualified as Os
#elif OSX
import Effects.Notify.Internal.Os.Osx qualified as Os
#else
import Effects.Notify.Internal.Os.Windows qualified as Os
#endif
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))

-- | Effect for sending desktop notifications.
--
-- @since 0.1
class (Monad m) => MonadNotify m where
  -- | Notification environment.
  --
  -- @since 0.1
  type NotifyEnvF m :: Type

  -- | Initialize the notification environment.
  --
  -- @since 0.1
  initNotifyEnv :: (HasCallStack) => NotifySystemOs -> m (NotifyEnvF m)

  -- | Send a notification.
  --
  -- @since 0.1
  notify :: (HasCallStack) => NotifyEnvF m -> Note -> m ()

-- | @since 0.1
instance MonadNotify IO where
  type NotifyEnvF IO = NotifyEnv

  initNotifyEnv = Os.initNotifyEnv
  {-# INLINEABLE initNotifyEnv #-}

  notify = Os.notify
  {-# INLINEABLE notify #-}

-- | @since 0.1
instance (MonadNotify m) => MonadNotify (ReaderT env m) where
  type NotifyEnvF (ReaderT env m) = NotifyEnvF m

  initNotifyEnv = lift . initNotifyEnv
  {-# INLINEABLE initNotifyEnv #-}

  notify env = lift . notify env
  {-# INLINEABLE notify #-}

-- | Runs 'notify' and runs the handler on any non-fatal 'NotifyException's.
-- Any other exceptions are rethrown.
--
-- @since 0.1
catchNonFatalNotify ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadNotify m
  ) =>
  -- | Env.
  NotifyEnvF m ->
  -- | Note.
  Note ->
  -- | Handler.
  (NotifyException -> m ()) ->
  m ()
catchNonFatalNotify env note handler =
  notify env note `C.catch` \ne ->
    if ne ^. #fatal
      then C.throwM ne
      else handler ne
{-# INLINEABLE catchNonFatalNotify #-}

-- | Like 'notify', but catches non-fatal 'NotifyException'.
--
-- @since 0.1
tryNonFatalNotify ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadNotify m
  ) =>
  -- | Env.
  NotifyEnvF m ->
  -- | Note.
  Note ->
  m (Maybe NotifyException)
tryNonFatalNotify env note =
  C.try (notify env note) >>= \case
    Left ex ->
      if ex ^. #fatal
        then C.throwM ex
        else pure $ Just ex
    Right _ -> pure Nothing
{-# INLINEABLE tryNonFatalNotify #-}

-- | Like 'tryNonFatalNotify', but catches and discards the exception.
--
-- @since 0.1
tryNonFatalNotify_ ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadNotify m
  ) =>
  -- | Env.
  NotifyEnvF m ->
  -- | Note.
  Note ->
  m ()
tryNonFatalNotify_ env = void . tryNonFatalNotify env
{-# INLINEABLE tryNonFatalNotify_ #-}

-- $desc
--
-- @effects-notify@ is a library for implementing cross-platform desktop
-- notifications. Currently supports:
--
-- - Linux: Supports @dbus@ and @notify-send@. That is, if there is a
--   running dbus notification server, it can be used directly.
--   @notify-send (libnotify)@ can also be used, though it also requires a
--   running dbus server.
--
-- - Osx: Supports built-in @apple-script@ i.e. @osascript@.
--
-- - Windows: Unsupported. Functions are mere stubs that will compile
--   (with a warning) and do nothing at runtime.
--
-- ==== Usage
--
-- General usage involves choosing the notification system and using that
-- to initialize the environment:
--
-- @
-- -- choose system
-- let systemOs :: 'NotifySystemOs'
--     systemOs = ...
--
-- -- prepare environment
-- notifyEnv <- 'initNotifyEnv' systemOs
--
-- -- send notification
-- let note =
--       'mkNote' "A summary"
--         & 'setBody' "Some notification"
--         & 'setTitle' "My Application"
--
-- 'notify' notifyEnv note
-- @
--
-- Choosing the system is more interesting. If we do not care about being
-- cross-platform, we can simply choose a 'NotifySystemOs' directly
-- and pass it to init:
--
-- @
-- -- The NotifySystemOsDBus constructor is only available on linux.
-- let systemOs :: 'NotifySystemOs'
--     systemOs = NotifySystemOsDBus
--
-- notifyEnv <- 'initNotifyEnv' systemOs
-- ...
-- @
--
-- If we need to work on several platforms, we can instead use 'NotifySystem',
-- which includes every possible notification system and is available on all
-- platforms. We then pass it to 'notifySystemToOs', which -- if successful --
-- will return the 'NotifySystemOs' and we can continue as normal.
--
-- @
-- -- All NotifySystem constructors are available on all platforms.
-- let system :: 'NotifySystem'
--     system = ...
--
-- systemOs <- 'notifySystemToOs' system \>\>= \\case
--   Left ex -> throwM ex
--   Right s -> pure s
-- notifyEnv <- 'initNotifyEnv' systemOs
-- ...
-- @
--
-- If a system is chosen that is unavailable on the current platform,
-- a 'NotifyParseException' will be returned.
