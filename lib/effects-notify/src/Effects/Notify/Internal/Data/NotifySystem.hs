{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.Notify.Internal.Data.NotifySystem
  ( -- * Platform-specific systems
    NotifySystemOs (..),
    defaultNotifySystemOs,

    -- * All systems
    NotifySystem (..),
    defaultNotifySystem,
    notifySystemToOs,
    notifySystemFromOs,

    -- * Exceptions
    NotifyParseException (..),
  )
where

{- ORMOLU_ENABLE -}

import Control.DeepSeq (NFData)
import Control.Monad.Catch (Exception (displayException), MonadThrow (throwM))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Display (Display (displayBuilder))
import Effects.Notify.Internal.Os (Os)
import Effects.Notify.Internal.Os qualified as Os
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics.Core (A_Lens, LabelOptic (labelOptic), (^.))
import Optics.Core qualified as O

-- NOTE: No need for any abstraction here as the only expected changes
-- (new system) would require a major bump anyway.

-- | Represents all possible notification systems. This is useful for when
-- we want to choose the system in some way that works across multiple
-- platforms without requiring cpp.
--
-- @since 0.1
data NotifySystem
  = NotifySystemAppleScript
  | NotifySystemDBus
  | NotifySystemNotifySend
  | NotifySystemWindows
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifySystem where
  displayBuilder = \case
    NotifySystemAppleScript -> "apple-script"
    NotifySystemDBus -> "dbus"
    NotifySystemNotifySend -> "notify-send"
    NotifySystemWindows -> "windows"

-- | Converts platform-agnostic 'NotifySystem' to platform-specific
-- 'NotifySystem'. Useful for obtaining a valid system for the current
-- platform without cpp.
--
-- @since 0.1
notifySystemToOs ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  NotifySystem ->
  m NotifySystemOs
{-# INLINEABLE notifySystemToOs #-}

-- | Inverse 'notifySystemToOs'.
--
-- @since 0.1
notifySystemFromOs :: NotifySystemOs -> NotifySystem

-- | @since 0.1
defaultNotifySystem :: NotifySystem

-- | @since 0.1
defaultNotifySystemOs :: NotifySystemOs

#if LINUX

notifySystemToOs = \case
  NotifySystemDBus -> pure NotifySystemOsDBus
  NotifySystemNotifySend -> pure NotifySystemOsNotifySend
  other -> throwM $ MkNotifyParseException Os.Linux other

notifySystemFromOs = \case
  NotifySystemOsDBus -> NotifySystemDBus
  NotifySystemOsNotifySend -> NotifySystemNotifySend

defaultNotifySystem = NotifySystemDBus

defaultNotifySystemOs = NotifySystemOsDBus

-- | Notification system. Options are platform specific.
--
-- @since 0.1
data NotifySystemOs
  = NotifySystemOsDBus
  | NotifySystemOsNotifySend
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifySystemOs where
  displayBuilder = displayBuilder . notifySystemFromOs

#elif OSX

notifySystemToOs = \case
  NotifySystemAppleScript -> pure NotifySystemOsAppleScript
  other -> throwM $ MkNotifyParseException Os.Osx other

notifySystemFromOs = \case
  NotifySystemOsAppleScript -> NotifySystemAppleScript

defaultNotifySystem = NotifySystemAppleScript

defaultNotifySystemOs = NotifySystemOsAppleScript

-- | Notification system. Options are platform specific.
--
-- @since 0.1
data NotifySystemOs = NotifySystemOsAppleScript
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifySystemOs where
  displayBuilder = displayBuilder . notifySystemFromOs

#else

notifySystemToOs = \case
  NotifySystemWindows -> pure NotifySystemOsWindows
  other -> throwM $ MkNotifyParseException Os.Windows other

notifySystemFromOs = \case
  NotifySystemOsWindows -> NotifySystemWindows

defaultNotifySystem = NotifySystemWindows

defaultNotifySystemOs = NotifySystemOsWindows

-- | Notification system. Options are platform specific.
--
-- @since 0.1
data NotifySystemOs = NotifySystemOsWindows
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifySystemOs where
  displayBuilder = displayBuilder . notifySystemFromOs

#endif

-- | Exceptions thrown when parsing 'NotifySystem' to 'NotifySystemOs'.
--
-- @since 0.1
data NotifyParseException = MkNotifyParseException
  { -- | The current 'Os'.
    os :: Os,
    -- | The 'NotifySystem' we attempted to use.
    system :: NotifySystem
  }
  deriving stock (Show)

-- | @since 0.1
instance Exception NotifyParseException where
  displayException ex =
    T.unpack
      . TBL.runBuilder
      . mconcat
      $ [ "Notification system '",
          displayBuilder (ex ^. #system),
          "' is unavailable on os ",
          displayBuilder (ex ^. #os),
          ". Available systems: ",
          availSystems,
          "."
        ]
    where
      availSystems =
        mconcat
          . L.intersperse ", "
          . fmap displayBuilder
          $ [minBound .. maxBound :: NotifySystemOs]

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Os, b ~ Os) =>
  LabelOptic "os" k NotifyParseException NotifyParseException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyParseException a1 a2) ->
    fmap (\b -> MkNotifyParseException b a2) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ NotifySystem, b ~ NotifySystem) =>
  LabelOptic "system" k NotifyParseException NotifyParseException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyParseException a1 a2) ->
    fmap (\b -> MkNotifyParseException a1 b) (f a2)
  {-# INLINE labelOptic #-}
