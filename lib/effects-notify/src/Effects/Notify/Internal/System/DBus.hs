{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | DBus notification data.
--
-- @since 0.1
module Effects.Notify.Internal.System.DBus
  ( -- * Notifications
    initNotifyEnv,
    notify,

    -- * Re-exports

    -- ** Errors
    ClientError,
    Client.clientError,
    Client.clientErrorMessage,
    Client.clientErrorFatal,
  )
where

import Control.Exception
  ( Exception (toException),
    SomeException,
    catch,
    throwIO,
  )
import Control.Exception.Utils qualified as Ex.Utils
import Control.Monad (void)
import DBus.Client (Client, ClientError)
import DBus.Client qualified as Client
import DBus.Notify qualified as DBusN
import Data.Int (Int32)
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
import Effects.Notify.Internal.Data.NotifyInitException
  ( NotifyInitException (MkNotifyInitException),
  )
import Effects.Notify.Internal.Data.NotifySystem (NotifySystem (NotifySystemDBus))
import Effects.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (NotifyTimeoutMillis, NotifyTimeoutNever),
  )
import Effects.Notify.Internal.Utils qualified as Utils
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))

-- | Initializes a dbus connections.
--
-- @since 0.1
initNotifyEnv :: (HasCallStack) => IO Client
initNotifyEnv =
  DBusN.connectSession
    `Ex.Utils.catchSync` (throwIO . MkNotifyInitException)

-- | Sends a notification via dbus.
--
-- @since 0.1
notify :: (HasCallStack) => Client -> Note -> IO ()
notify client note =
  sendNote note
    `catch` \ex ->
      -- According to the docs, toException drops the exception context,
      -- but we should be okay here since 'catch' annotates our new exception
      -- with the context (WhileHandling origEx), so the context should
      -- have the original stacktrace, while our NotifyInitException will
      -- wrap an contextless SomeException, which is fine.
      --
      -- Alternatively, we could use toExceptionWithBacktrace, but I think
      -- that would be redundant? It would be nice to check this.
      throwEx (Client.clientErrorFatal ex) (toException ex)
        `Ex.Utils.catchSync` \someEx -> throwEx True someEx
  where
    sendNote = void . DBusN.notify client . noteToDbus

    throwEx :: Bool -> SomeException -> IO ()
    throwEx fatal ex =
      throwIO $
        MkNotifyException
          { exception = toException ex,
            fatal,
            note,
            notifySystem = NotifySystemDBus
          }

noteToDbus :: Note -> DBusN.Note
noteToDbus note =
  DBusN.Note
    { DBusN.appName = maybe "" T.unpack (note ^. #title),
      DBusN.body = DBusN.Text . T.unpack <$> note ^. #body,
      DBusN.summary = T.unpack $ note ^. #summary,
      DBusN.appImage = Nothing,
      DBusN.hints = [],
      DBusN.expiry,
      DBusN.actions = []
    }
  where
    expiry = case note ^. #timeout of
      -- I think this is the default?
      Nothing -> DBusN.Dependent
      Just NotifyTimeoutNever -> DBusN.Never
      Just (NotifyTimeoutMillis s) ->
        DBusN.Milliseconds (Utils.unsafeConvertIntegral @Int @Int32 s)
