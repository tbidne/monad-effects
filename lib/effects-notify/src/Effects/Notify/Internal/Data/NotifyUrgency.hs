module Effects.Notify.Internal.Data.NotifyUrgency
  ( NotifyUrgency (..),
    _NotifyUrgencyLow,
    _NotifyUrgencyNormal,
    _NotifyUrgencyCritical,
  )
where

import Control.DeepSeq (NFData)
import Data.Text.Display (Display (displayBuilder))
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)

-- | Notification urgency. Not supported by all notification systems.
data NotifyUrgency
  = -- | Low.
    NotifyUrgencyLow
  | -- | Normal.
    NotifyUrgencyNormal
  | -- | Critical.
    NotifyUrgencyCritical
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifyUrgency where
  displayBuilder = \case
    NotifyUrgencyLow -> "low"
    NotifyUrgencyNormal -> "normal"
    NotifyUrgencyCritical -> "critical"

-- | @since 0.1
_NotifyUrgencyLow :: Prism' NotifyUrgency ()
_NotifyUrgencyLow =
  prism
    (const NotifyUrgencyLow)
    ( \case
        NotifyUrgencyLow -> Right ()
        x -> Left x
    )
{-# INLINE _NotifyUrgencyLow #-}

-- | @since 0.1
_NotifyUrgencyNormal :: Prism' NotifyUrgency ()
_NotifyUrgencyNormal =
  prism
    (const NotifyUrgencyNormal)
    ( \case
        NotifyUrgencyNormal -> Right ()
        x -> Left x
    )
{-# INLINE _NotifyUrgencyNormal #-}

-- | @since 0.1
_NotifyUrgencyCritical :: Prism' NotifyUrgency ()
_NotifyUrgencyCritical =
  prism
    (const NotifyUrgencyCritical)
    ( \case
        NotifyUrgencyCritical -> Right ()
        x -> Left x
    )
{-# INLINE _NotifyUrgencyCritical #-}
