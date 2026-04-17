module Effects.Notify.Internal.Data.NotifyTimeout
  ( NotifyTimeout (..),
    _NotifyTimeoutMillis,
    _NotifyTimeoutNever,
  )
where

import Control.DeepSeq (NFData)
import Data.Text.Display (Display (displayBuilder))
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)

-- | Notification timeout, in milliseconds.
--
-- @since 0.1
data NotifyTimeout
  = NotifyTimeoutNever
  | NotifyTimeoutMillis Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | @since 0.1
instance Display NotifyTimeout where
  displayBuilder = \case
    NotifyTimeoutNever -> "never"
    NotifyTimeoutMillis i -> displayBuilder i <> " ms"

-- | @since 0.1
_NotifyTimeoutMillis :: Prism' NotifyTimeout Int
_NotifyTimeoutMillis =
  prism
    NotifyTimeoutMillis
    ( \case
        NotifyTimeoutMillis x -> Right x
        x -> Left x
    )
{-# INLINE _NotifyTimeoutMillis #-}

-- | @since 0.1
_NotifyTimeoutNever :: Prism' NotifyTimeout ()
_NotifyTimeoutNever =
  prism
    (const NotifyTimeoutNever)
    ( \case
        NotifyTimeoutNever -> Right ()
        x -> Left x
    )
{-# INLINE _NotifyTimeoutNever #-}
