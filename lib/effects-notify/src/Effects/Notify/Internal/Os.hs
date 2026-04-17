module Effects.Notify.Internal.Os
  ( Os (..),
  )
where

import Data.Text.Display (Display (displayBuilder))

-- | Os types.
--
-- @since 0.1
data Os
  = -- | @since 0.1
    Linux
  | -- | @since 0.1
    Osx
  | -- | @since 0.1
    Windows
  deriving stock (Bounded, Eq, Show)

-- | @since 0.1
instance Display Os where
  displayBuilder = \case
    Linux -> "linux"
    Osx -> "osx"
    Windows -> "windows"
