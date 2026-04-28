{-# LANGUAGE CPP #-}

-- | Notify-send notification data.
--
-- @since 0.1
module Effects.Notify.Internal.System.NotifySend
  ( notify,
  )
where

import Control.Exception (throwIO)
import Control.Exception.Utils qualified as Ex.Utils
import Data.Text qualified as T
import Effects.Notify.Internal.Data.Note (Note)
import Effects.Notify.Internal.Data.NotifyException
  ( NotifyException
      ( MkNotifyException,
        exception,
        fatal,
        note,
        notifySystem
      ),
  )
import Effects.Notify.Internal.Data.NotifySystem
  ( NotifySystem (NotifySystemNotifySend),
  )
import Effects.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (NotifyTimeoutMillis, NotifyTimeoutNever),
  )
import Effects.Notify.Internal.Data.NotifyUrgency
  ( NotifyUrgency (NotifyUrgencyCritical, NotifyUrgencyLow, NotifyUrgencyNormal),
  )
import Effects.Notify.Internal.Utils qualified as Utils
import Optics.Core ((^.))

-- | Sends a notification via notify-send.
--
-- @since 0.1
notify :: Note -> IO ()
notify note =
  sendNote note `Ex.Utils.catchSync` \ex ->
    throwIO $
      MkNotifyException
        { exception = ex,
          fatal = True,
          note,
          notifySystem = NotifySystemNotifySend
        }
  where
    sendNote = Utils.runProcessIO . noteToNotifySend

noteToNotifySend :: Note -> String
noteToNotifySend note =
  T.unpack
    . mconcat
    $ [ "notify-send ",
        maybe "" ((" --app-name " <>) . Utils.mkProcessText) (note ^. #title),
        Utils.mkProcessText $ note ^. #summary,
        maybe "" Utils.mkProcessText (note ^. #body),
        maybe "" ((" --urgency " <>) . ulToNS) (note ^. #urgency),
        maybe "" ((" --expire-time " <>) . toToNS) (note ^. #timeout)
      ]
  where
    ulToNS NotifyUrgencyLow = "low"
    ulToNS NotifyUrgencyNormal = "normal"
    ulToNS NotifyUrgencyCritical = "critical"

    toToNS NotifyTimeoutNever = "0"
    toToNS (NotifyTimeoutMillis n) = Utils.showt n
