-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.MonadThread
  ( -- * Threads
    MonadThread (..),
    microsleep,
    sleep,

    -- * Semaphores
    MonadQSem (..),

    -- * Reexports
    Natural,
    ThreadId,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent qualified as CC
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
import Control.Exception (Exception)
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
  -- | Lifted 'CC.threadDelay'.
  --
  -- @since 0.1
  threadDelay :: HasCallStack => Int -> m ()

  -- | Lifted 'CC.throwTo'.
  --
  -- @since 0.1
  throwTo :: (Exception e, HasCallStack) => ThreadId -> e -> m ()

  -- | Lifted 'CC.getNumCapabilities'
  --
  -- @since 0.1
  getNumCapabilities :: HasCallStack => m Int

  -- | Lifted 'CC.setNumCapabilities'
  --
  -- @since 0.1
  setNumCapabilities :: HasCallStack => Int -> m ()

  -- | Lifted 'CC.threadCapability'
  --
  -- @since 0.1
  threadCapability :: HasCallStack => ThreadId -> m (Int, Bool)

-- | @since 0.1
instance MonadThread IO where
  threadDelay = addCallStack . CC.threadDelay
  {-# INLINEABLE threadDelay #-}
  throwTo tid = addCallStack . CC.throwTo tid
  {-# INLINEABLE throwTo #-}
  getNumCapabilities = addCallStack CC.getNumCapabilities
  {-# INLINEABLE getNumCapabilities #-}
  setNumCapabilities = addCallStack . CC.setNumCapabilities
  {-# INLINEABLE setNumCapabilities #-}
  threadCapability = addCallStack . CC.threadCapability
  {-# INLINEABLE threadCapability #-}

-- | @since 0.1
instance MonadThread m => MonadThread (ReaderT e m) where
  threadDelay = lift . threadDelay
  {-# INLINEABLE threadDelay #-}
  throwTo tid = lift . throwTo tid
  {-# INLINEABLE throwTo #-}
  getNumCapabilities = lift getNumCapabilities
  {-# INLINEABLE getNumCapabilities #-}
  setNumCapabilities = lift . setNumCapabilities
  {-# INLINEABLE setNumCapabilities #-}
  threadCapability = lift . threadCapability
  {-# INLINEABLE threadCapability #-}

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int'.
--
-- @since 0.1
microsleep :: (HasCallStack, MonadThread m) => Natural -> m ()
microsleep n = for_ (natToInts n) threadDelay
{-# INLINEABLE microsleep #-}

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (HasCallStack, MonadThread m) => Natural -> m ()
sleep = microsleep . (* 1_000_000)
{-# INLINEABLE sleep #-}

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
  -- | Lifted 'QSem.newQSem'.
  --
  -- @since 0.1
  newQSem :: Int -> m QSem

  -- | Lifted 'QSem.waitQSem'.
  --
  -- @since 0.1
  waitQSem :: QSem -> m ()

  -- | Lifted 'QSem.signalQSem'.
  --
  -- @since 0.1
  signalQSem :: QSem -> m ()

  -- | Lifted 'QSemN.newQSemN'.
  --
  -- @since 0.1
  newQSemN :: Int -> m QSemN

  -- | Lifted 'QSemN.waitQSemN'.
  --
  -- @since 0.1
  waitQSemN :: QSemN -> Int -> m ()

  -- | Lifted 'QSemN.signalQSemN'.
  --
  -- @since 0.1
  signalQSemN :: QSemN -> Int -> m ()

-- | @since 0.1
instance MonadQSem IO where
  newQSem = QSem.newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = QSem.waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = QSem.signalQSem
  {-# INLINEABLE signalQSem #-}
  newQSemN = QSemN.newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN = QSemN.waitQSemN
  {-# INLINEABLE waitQSemN #-}
  signalQSemN = QSemN.signalQSemN
  {-# INLINEABLE signalQSemN #-}

-- | @since 0.1
instance MonadQSem m => MonadQSem (ReaderT e m) where
  newQSem = lift . newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = lift . waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = lift . signalQSem
  {-# INLINEABLE signalQSem #-}
  newQSemN = lift . newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN q = lift . waitQSemN q
  {-# INLINEABLE waitQSemN #-}
  signalQSemN q = lift . signalQSemN q
  {-# INLINEABLE signalQSemN #-}
