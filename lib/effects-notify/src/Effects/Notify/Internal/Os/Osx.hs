{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.Notify.Internal.Os.Osx
  ( initNotifyEnv,
    notify,
  )
where

import Effects.Notify.Internal.Data.Note (Note)
import Effects.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (NotifyEnvAppleScript),
  )
import Effects.Notify.Internal.Data.NotifySystem
  ( NotifySystemOs (NotifySystemOsAppleScript),
  )
import Effects.Notify.Internal.System.AppleScript qualified as AppleScript
import GHC.Stack.Types (HasCallStack)

initNotifyEnv :: (HasCallStack) => NotifySystemOs -> IO NotifyEnv
initNotifyEnv = \case
  NotifySystemOsAppleScript -> pure NotifyEnvAppleScript

notify :: NotifyEnv -> Note -> IO ()
notify env note = case env of
  NotifyEnvAppleScript -> AppleScript.notify note
