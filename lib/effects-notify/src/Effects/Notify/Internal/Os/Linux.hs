module Effects.Notify.Internal.Os.Linux
  ( initNotifyEnv,
    notify,
  )
where

import Effects.Notify.Internal.Data.Note (Note)
import Effects.Notify.Internal.Data.NotifyEnv
  ( NotifyEnv (NotifyEnvDBus, NotifyEnvNotifySend),
  )
import Effects.Notify.Internal.Data.NotifySystem
  ( NotifySystemOs (NotifySystemOsDBus, NotifySystemOsNotifySend),
  )
import Effects.Notify.Internal.System.DBus qualified as DBus
import Effects.Notify.Internal.System.NotifySend qualified as NotifySend
import GHC.Stack.Types (HasCallStack)

initNotifyEnv :: (HasCallStack) => NotifySystemOs -> IO NotifyEnv
initNotifyEnv = \case
  NotifySystemOsDBus -> NotifyEnvDBus <$> DBus.initNotifyEnv
  NotifySystemOsNotifySend -> pure NotifyEnvNotifySend

notify :: NotifyEnv -> Note -> IO ()
notify env note = case env of
  NotifyEnvDBus client -> DBus.notify client note
  NotifyEnvNotifySend -> NotifySend.notify note
