{-# LANGUAGE CPP #-}

module Effects.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (..),
    notifyEnvToSystemOs,
  )
where

#if LINUX
import DBus.Client (Client)
#endif
import Data.Text.Display (Display (displayBuilder))
import Effects.Notify.Internal.Data.NotifySystem (NotifySystemOs)
import Effects.Notify.Internal.Data.NotifySystem qualified as System

-- NOTE: NotifyEnv will be abstract (i.e. unexported from public interface),
-- though we need to export its constructors here so other internal modules
-- here can use it.
--
-- There is no need to export anything else.

-- | Gives the system for the given environment.
--
-- @since 0.1
notifyEnvToSystemOs :: NotifyEnv -> NotifySystemOs

#if LINUX

-- | Notification environment.
--
-- @since 0.1
data NotifyEnv
  = NotifyEnvDBus Client
  | NotifyEnvNotifySend

-- | @since 0.1`
instance Show NotifyEnv where
  show (NotifyEnvDBus _) = "NotifyEnvDBus"
  show NotifyEnvNotifySend = "NotifyEnvNotifySend"

-- | @since 0.1
instance Display NotifyEnv where
  displayBuilder = \case
    NotifyEnvDBus _ -> "dbus"
    NotifyEnvNotifySend -> "notify-send"

notifyEnvToSystemOs = \case
  NotifyEnvDBus _ -> System.NotifySystemOsDBus
  NotifyEnvNotifySend -> System.NotifySystemOsNotifySend

#elif OSX

-- | Notification environment.
--
-- @since 0.1
data NotifyEnv = NotifyEnvAppleScript

-- | @since 0.1
instance Show NotifyEnv where
  show NotifyEnvAppleScript = "NotifyEnvAppleScript"

-- | @since 0.1
instance Display NotifyEnv where
  displayBuilder = \case
    NotifyEnvAppleScript -> "apple-script"

notifyEnvToSystemOs = \case
  NotifyEnvAppleScript -> System.NotifySystemOsAppleScript

#else

-- | Notification environment.
--
-- @since 0.1
data NotifyEnv = NotifyEnvWindows

-- | @since 0.1
instance Show NotifyEnv where
  show NotifyEnvWindows = "NotifyEnvWindows"

-- | @since 0.1
instance Display NotifyEnv where
  displayBuilder = \case
    NotifyEnvWindows -> "windows"

notifyEnvToSystemOs = \case
  NotifyEnvWindows -> System.NotifySystemOsWindows

#endif
