{-# LANGUAGE CPP #-}

module Unit.NotifySystem (tests) where

import Control.Monad.Catch (Exception (displayException))
import Data.Bifunctor (first)
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
  Left e1 @=? k Notify.NotifySystemAppleScript
  Right Notify.NotifySystemOsDBus @=? k Notify.NotifySystemDBus
  Right Notify.NotifySystemOsNotifySend @=? k Notify.NotifySystemNotifySend
  Left e2 @=? k Notify.NotifySystemWindows
#elif OSX
  Right Notify.NotifySystemOsAppleScript @=? k Notify.NotifySystemAppleScript
  Left e1 @=? k Notify.NotifySystemDBus
  Left e2 @=? k Notify.NotifySystemNotifySend
  Left e3 @=? k Notify.NotifySystemWindows
#else
  Left e1 @=? k Notify.NotifySystemAppleScript
  Left e2 @=? k Notify.NotifySystemDBus
  Left e3 @=? k Notify.NotifySystemNotifySend
  Right Notify.NotifySystemOsWindows @=? k Notify.NotifySystemWindows
#endif
  where
    desc = "notifySystemToOs"
    k = first displayException . Notify.notifySystemToOs

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
