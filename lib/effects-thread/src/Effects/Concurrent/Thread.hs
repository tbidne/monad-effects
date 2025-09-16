{-# LANGUAGE CPP #-}

-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.Concurrent.Thread
  ( -- * Thread Effect
    MonadThread (..),
    microsleep,
    sleep,

    -- * Reexports
    Natural,
    ThreadId,

    -- * QSem Effect
    MonadQSem (..),
    MonadQSemN (..),

    -- * Reexports
    QSem,
    QSemN,
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
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Foldable (for_)
import GHC.Conc.Sync qualified as Sync
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

{- ORMOLU_DISABLE -}

-- | Represents thread effects.
--
-- @since 0.1
class (Monad m) => MonadThread m where
  -- | Lifted 'CC.threadDelay'.
  --
  -- @since 0.1
  threadDelay :: (HasCallStack) => Int -> m ()

  -- | Lifted 'CC.forkIO'.
  --
  -- @since 0.1
  forkM :: (HasCallStack) => m () -> m ThreadId

  -- | Lifted 'CC.throwTo'.
  --
  -- @since 0.1
  throwTo :: (Exception e, HasCallStack) => ThreadId -> e -> m ()

  -- | Lifted 'CC.getNumCapabilities'.
  --
  -- @since 0.1
  getNumCapabilities :: (HasCallStack) => m Int

  -- | Lifted 'CC.setNumCapabilities'.
  --
  -- @since 0.1
  setNumCapabilities :: (HasCallStack) => Int -> m ()

  -- | Lifted 'CC.threadCapability'.
  --
  -- @since 0.1
  threadCapability :: (HasCallStack) => ThreadId -> m (Int, Bool)

  -- | Lifted 'CC.myThreadId'.
  --
  -- @since 0.1
  myThreadId :: (HasCallStack) => m ThreadId

  -- | Lifted 'Sync.labelThread'.
  --
  -- @since 0.1
  labelThread :: (HasCallStack) => ThreadId -> String -> m ()

#if MIN_VERSION_base(4, 18, 0)

  -- | Lifted 'Sync.threadLabel'.
  --
  -- @since 0.1
  threadLabel :: (HasCallStack) => ThreadId -> m (Maybe String)

#endif

-- | @since 0.1
instance MonadThread IO where
  threadDelay = CC.threadDelay
  {-# INLINEABLE threadDelay #-}
  throwTo = CC.throwTo
  {-# INLINEABLE throwTo #-}
  getNumCapabilities = CC.getNumCapabilities
  {-# INLINEABLE getNumCapabilities #-}
  setNumCapabilities = CC.setNumCapabilities
  {-# INLINEABLE setNumCapabilities #-}
  threadCapability = CC.threadCapability
  {-# INLINEABLE threadCapability #-}
  myThreadId = CC.myThreadId
  {-# INLINEABLE myThreadId #-}
  forkM = CC.forkIO
  {-# INLINEABLE forkM #-}
  labelThread = Sync.labelThread
  {-# INLINEABLE labelThread #-}
#if MIN_VERSION_base(4, 18, 0)
  threadLabel = Sync.threadLabel
  {-# INLINEABLE threadLabel #-}
#endif

-- | @since 0.1
instance (MonadThread m) => MonadThread (ReaderT e m) where
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
  myThreadId = lift myThreadId
  {-# INLINEABLE myThreadId #-}
  forkM m = ask >>= \e -> lift (forkM (runReaderT m e))
  {-# INLINEABLE forkM #-}
  labelThread tid = lift . labelThread tid
  {-# INLINEABLE labelThread #-}
#if MIN_VERSION_base(4, 18, 0)
  threadLabel = lift . threadLabel
  {-# INLINEABLE threadLabel #-}
#endif

{- ORMOLU_ENABLE -}

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
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

-- | Effect for 'QSem' semaphore.
--
-- @since 0.1
class (Monad m) => MonadQSem m where
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

-- | @since 0.1
instance MonadQSem IO where
  newQSem = QSem.newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = QSem.waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = QSem.signalQSem
  {-# INLINEABLE signalQSem #-}

-- | @since 0.1
instance (MonadQSem m) => MonadQSem (ReaderT e m) where
  newQSem = lift . newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = lift . waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = lift . signalQSem
  {-# INLINEABLE signalQSem #-}

-- | Effect for 'QSemN' semaphore.
--
-- @since 0.1
class (Monad m) => MonadQSemN m where
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
instance MonadQSemN IO where
  newQSemN = QSemN.newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN = QSemN.waitQSemN
  {-# INLINEABLE waitQSemN #-}
  signalQSemN = QSemN.signalQSemN
  {-# INLINEABLE signalQSemN #-}

-- | @since 0.1
instance (MonadQSemN m) => MonadQSemN (ReaderT e m) where
  newQSemN = lift . newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN q = lift . waitQSemN q
  {-# INLINEABLE waitQSemN #-}
  signalQSemN q = lift . signalQSemN q
  {-# INLINEABLE signalQSemN #-}
