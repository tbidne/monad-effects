-- | Notify-send notification data.
--
-- @since 0.1
module Effects.Notify.System.NotifySend
  ( -- * Notifications
    I.NotifySend.notify,
  )
where

import Effects.Notify.Internal.System.NotifySend qualified as I.NotifySend
