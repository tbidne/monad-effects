{-# LANGUAGE CPP #-}

module Effects.Notify.Internal.System.AppleScript
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
  ( NotifySystem (NotifySystemAppleScript),
  )
import Effects.Notify.Internal.Utils qualified as Utils
import Optics.Core ((^.))

notify :: Note -> IO ()
notify note =
  sendNote note `Ex.Utils.catchSync` \ex ->
    throwIO $
      MkNotifyException
        { exception = ex,
          fatal = True,
          note,
          notifySystem = NotifySystemAppleScript
        }
  where
    sendNote = Utils.runProcessIO . noteToAppleScript

noteToAppleScript :: Note -> String
noteToAppleScript note =
  T.unpack
    . mconcat
    $ [ "osascript -e 'display notification ",
        maybe "" (Utils.mkProcessTextQuote True) (note ^. #body),
        maybe "" ((" with title " <>) . Utils.mkProcessTextQuote True) (note ^. #title),
        " subtitle ",
        Utils.mkProcessTextQuote True (note ^. #summary),
        "'"
      ]
