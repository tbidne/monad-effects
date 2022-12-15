-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.MonadThread
  ( -- * Threads
    MonadThread (..),
    sleep,

    -- * Semaphores
    MonadQSem (..),

    -- * Reexports
    Natural,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Effects.MonadCallStack (addCallStack)
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
  microsleep n = addCallStack $ for_ (natToInts n) threadDelay

-- | @since 0.1
instance MonadThread m => MonadThread (ReaderT e m) where
  microsleep = lift . microsleep

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (HasCallStack, MonadThread m) => Natural -> m ()
sleep = microsleep . (* 1_000_000)

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

-- | Effect for simple semaphores.
--
-- @since 0.1
class Monad m => MonadQSem m where
  -- | Creates a 'QSem'.
  --
  -- @since 0.1
  newQSem :: Int -> m QSem

  -- | Waits for a 'QSem'.
  --
  -- @since 0.1
  waitQSem :: QSem -> m ()

  -- | Signals a 'QSem'.
  --
  -- @since 0.1
  signalQSem :: QSem -> m ()

  -- | Creates a 'QSemN'.
  --
  -- @since 0.1
  newQSemN :: Int -> m QSemN

  -- | Waits for a 'QSemN'.
  --
  -- @since 0.1
  waitQSemN :: QSemN -> Int -> m ()

  -- | Signals a 'QSemN'.
  --
  -- @since 0.1
  signalQSemN :: QSemN -> Int -> m ()

instance MonadQSem IO where
  newQSem = QSem.newQSem
  waitQSem = QSem.waitQSem
  signalQSem = QSem.signalQSem
  newQSemN = QSemN.newQSemN
  waitQSemN = QSemN.waitQSemN
  signalQSemN = QSemN.signalQSemN
