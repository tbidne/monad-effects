-- | DBus notification data.
--
-- @since 0.1
module Effects.Notify.System.DBus
  ( -- * Notifications
    I.DBus.initNotifyEnv,
    I.DBus.notify,

    -- * Re-exports

    -- ** Errors
    I.DBus.ClientError,
    I.DBus.clientError,
    I.DBus.clientErrorMessage,
    I.DBus.clientErrorFatal,
  )
where

import Effects.Notify.Internal.System.DBus qualified as I.DBus
