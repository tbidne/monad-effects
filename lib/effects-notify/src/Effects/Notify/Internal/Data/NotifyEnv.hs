{-# LANGUAGE CPP #-}

module Effects.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (..),
  )
where

#if LINUX
import DBus.Client (Client)
#endif
import Data.Text.Display (Display (displayBuilder))

-- NOTE: NotifyEnv will be abstract (i.e. unexported from public interface),
-- though we need to export its constructors here so other internal modules
-- here can use it.
--
-- There is no need to export anything else.

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

#endif
