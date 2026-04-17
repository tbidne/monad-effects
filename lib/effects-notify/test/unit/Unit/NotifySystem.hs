{-# LANGUAGE CPP #-}

module Unit.NotifySystem (tests) where

import Control.Monad.Catch
  ( Exception (displayException),
    MonadThrow (throwM),
  )
import Effects.Notify qualified as Notify
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "NotifySystem"
    [ testNotifySystemToOs
    ]

{- ORMOLU_DISABLE -}

testNotifySystemToOs :: TestTree
testNotifySystemToOs = testCase desc $ do
#if LINUX
  EL e1 @=? k Notify.NotifySystemAppleScript
  ER Notify.NotifySystemOsDBus @=? k Notify.NotifySystemDBus
  ER Notify.NotifySystemOsNotifySend @=? k Notify.NotifySystemNotifySend
  EL e2 @=? k Notify.NotifySystemWindows
#elif OSX
  ER Notify.NotifySystemOsAppleScript @=? k Notify.NotifySystemAppleScript
  EL e1 @=? k Notify.NotifySystemDBus
  EL e2 @=? k Notify.NotifySystemNotifySend
  EL e3 @=? k Notify.NotifySystemWindows
#else
  EL e1 @=? k Notify.NotifySystemAppleScript
  EL e2 @=? k Notify.NotifySystemDBus
  EL e3 @=? k Notify.NotifySystemNotifySend
  ER Notify.NotifySystemOsWindows @=? k Notify.NotifySystemWindows
#endif
  where
    desc = "notifySystemToOs"
    k = Notify.notifySystemToOs @EitherString

#if LINUX
    e1 = "Notification system 'apple-script' is unavailable on os linux. Available systems: dbus, notify-send."
    e2 = "Notification system 'windows' is unavailable on os linux. Available systems: dbus, notify-send."
#elif OSX
    e1 = "Notification system 'dbus' is unavailable on os osx. Available systems: apple-script."
    e2 = "Notification system 'notify-send' is unavailable on os osx. Available systems: apple-script."
    e3 = "Notification system 'windows' is unavailable on os osx. Available systems: apple-script."
#else
    e1 = "Notification system 'apple-script' is unavailable on os windows. Available systems: windows."
    e2 = "Notification system 'dbus' is unavailable on os windows. Available systems: windows."
    e3 = "Notification system 'notify-send' is unavailable on os windows. Available systems: windows."
#endif

{- ORMOLU_ENABLE -}

data EitherString a
  = EL String
  | ER a
  deriving stock (Eq, Functor, Show)

instance Applicative EitherString where
  pure = ER

  EL s <*> _ = EL s
  _ <*> EL s = EL s
  ER f <*> ER x = ER (f x)

instance Monad EitherString where
  EL s >>= _ = EL s
  ER x >>= k = k x

instance MonadThrow EitherString where
  throwM = EL . displayException
