{-# LANGUAGE UndecidableInstances #-}

module Effects.Notify.Internal.Data.NotifyInitException
  ( NotifyInitException (..),
  )
where

import Control.Exception (Exception (displayException), SomeException)

-- | Exceptions thrown when initializing notifications.
--
-- @since 0.1
newtype NotifyInitException = MkNotifyInitException
  { unNotifyInitException :: SomeException
  }

-- | @since 0.1
deriving stock instance Show NotifyInitException

-- | @since 0.1
instance Exception NotifyInitException where
  displayException ex =
    mconcat
      [ "Exception initializing notifications: ",
        displayException ex
      ]
