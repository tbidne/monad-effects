-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.MonadThread
  ( MonadThread (..),
    sleep,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Effects.MonadCallStack (MonadCallStack, checkpointCallStack)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

-- | Represents thread effects.
--
-- @since 0.1
class Monad m => MonadThread m where
  -- | Runs sleep in the current thread for the specified number of
  -- microseconds.
  --
  -- @since 0.1
  microsleep :: HasCallStack => Natural -> m ()

-- | @since 0.1
instance MonadThread IO where
  microsleep n = checkpointCallStack $ for_ (natToInts n) threadDelay

-- | @since 0.1
instance MonadThread m => MonadThread (ReaderT e m) where
  microsleep = lift . microsleep

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (HasCallStack, MonadCallStack m, MonadThread m) => Natural -> m ()
sleep = checkpointCallStack . microsleep . (* 1_000_000)

natToInts :: Natural -> [Int]
natToInts n
  | n > maxIntAsNat = maxInt : natToInts (n - maxIntAsNat)
  | otherwise = [n2i n]
  where
    maxInt :: Int
    maxInt = maxBound
    maxIntAsNat :: Natural
    maxIntAsNat = i2n maxInt

n2i :: Natural -> Int
n2i = fromIntegral

i2n :: Int -> Natural
i2n = fromIntegral
