{-# LANGUAGE CPP #-}

-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.Concurrent.Thread
  ( -- * Thread Effect
    MonadThread (..),
    microsleep,
    sleep,

    -- ** Reexports
    Natural,
    ThreadId,

    -- * MVar Effect
    MonadMVar (..),

    -- ** Reexports
    MVar,

    -- * QSem Effect
    MonadQSem (..),
    MonadQSemN (..),

    -- ** Reexports
    QSem,
    QSemN,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent qualified as CC
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
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
import System.Mem.Weak (Weak)

{- ORMOLU_DISABLE -}

-- | Represents thread effects.
--
-- @since 0.1
class (Monad m) => MonadThread m where
  -- | Lifted 'CC.threadDelay'.
  --
  -- @since 0.1
  threadDelay :: (HasCallStack) => Int -> m ()

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

-- | Effect for 'MVar'.
--
-- @since 0.1
class (Monad m) => MonadMVar m where
  -- | Lifted 'MVar.newEmptyMVar'.
  --
  -- @since 0.1
  newEmptyMVar :: m (MVar a)

  -- | Lifted 'MVar.newMVar'.
  --
  -- @since 0.1
  newMVar :: a -> m (MVar a)

  -- | Lifted 'MVar.takeMVar'.
  --
  -- @since 0.1
  takeMVar :: MVar a -> m a

  -- | Lifted 'MVar.putMVar'.
  --
  -- @since 0.1
  putMVar :: MVar a -> a -> m ()

  -- | Lifted 'MVar.tryTakeMVar'.
  --
  -- @since 0.1
  tryTakeMVar :: MVar a -> m (Maybe a)

  -- | Lifted 'MVar.tryPutMVar'.
  --
  -- @since 0.1
  tryPutMVar :: MVar a -> a -> m Bool

  -- | Lifted 'MVar.isEmptyMVar'.
  --
  -- @since 0.1
  isEmptyMVar :: MVar a -> m Bool

  -- | Lifted 'MVar.withMVar'.
  --
  -- @since 0.1
  withMVar :: MVar a -> (a -> m b) -> m b

  -- | Lifted 'MVar.withMVarMasked'.
  --
  -- @since 0.1
  withMVarMasked :: MVar a -> (a -> m b) -> m b

  -- | Lifted 'MVar.modifyMVar_'.
  --
  -- @since 0.1
  modifyMVar_ :: MVar a -> (a -> m a) -> m ()

  -- | Lifted 'MVar.modifyMVar'.
  --
  -- @since 0.1
  modifyMVar :: MVar a -> (a -> m (a, b)) -> m b

  -- | Lifted 'MVar.modifyMVarMasked_'.
  --
  -- @since 0.1
  modifyMVarMasked_ :: MVar a -> (a -> m a) -> m ()

  -- | Lifted 'MVar.modifyMVarMasked'.
  --
  -- @since 0.1
  modifyMVarMasked :: MVar a -> (a -> m (a, b)) -> m b

  -- | Lifted 'MVar.tryReadMVar'.
  --
  -- @since 0.1
  tryReadMVar :: MVar a -> m (Maybe a)

  -- | Lifted 'MVar.mkWeakMVar'.
  --
  -- @since 0.1
  mkWeakMVar :: MVar a -> m () -> m (Weak (MVar a))

-- | @since 0.1
instance MonadMVar IO where
  newEmptyMVar = MVar.newEmptyMVar
  {-# INLINEABLE newEmptyMVar #-}
  newMVar = MVar.newMVar
  {-# INLINEABLE newMVar #-}
  takeMVar = MVar.takeMVar
  {-# INLINEABLE takeMVar #-}
  putMVar = MVar.putMVar
  {-# INLINEABLE putMVar #-}
  tryTakeMVar = MVar.tryTakeMVar
  {-# INLINEABLE tryTakeMVar #-}
  tryPutMVar = MVar.tryPutMVar
  {-# INLINEABLE tryPutMVar #-}
  isEmptyMVar = MVar.isEmptyMVar
  {-# INLINEABLE isEmptyMVar #-}
  withMVar = MVar.withMVar
  {-# INLINEABLE withMVar #-}
  withMVarMasked = MVar.withMVarMasked
  {-# INLINEABLE withMVarMasked #-}
  modifyMVar_ = MVar.modifyMVar_
  {-# INLINEABLE modifyMVar_ #-}
  modifyMVar = MVar.modifyMVar
  {-# INLINEABLE modifyMVar #-}
  modifyMVarMasked_ = MVar.modifyMVarMasked_
  {-# INLINEABLE modifyMVarMasked_ #-}
  modifyMVarMasked = MVar.modifyMVarMasked
  {-# INLINEABLE modifyMVarMasked #-}
  tryReadMVar = MVar.tryReadMVar
  {-# INLINEABLE tryReadMVar #-}
  mkWeakMVar = MVar.mkWeakMVar
  {-# INLINEABLE mkWeakMVar #-}

-- | @since 0.1
instance (MonadMVar m) => MonadMVar (ReaderT e m) where
  newEmptyMVar = lift newEmptyMVar
  {-# INLINEABLE newEmptyMVar #-}
  newMVar = lift . newMVar
  {-# INLINEABLE newMVar #-}
  takeMVar = lift . takeMVar
  {-# INLINEABLE takeMVar #-}
  putMVar x1 = lift . putMVar x1
  {-# INLINEABLE putMVar #-}
  tryTakeMVar = lift . tryTakeMVar
  {-# INLINEABLE tryTakeMVar #-}
  tryPutMVar x1 = lift . tryPutMVar x1
  {-# INLINEABLE tryPutMVar #-}
  isEmptyMVar = lift . isEmptyMVar
  {-# INLINEABLE isEmptyMVar #-}
  withMVar var onVar =
    ask >>= \e ->
      lift $ withMVar var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE withMVar #-}
  withMVarMasked var onVar =
    ask >>= \e ->
      lift $ withMVarMasked var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE withMVarMasked #-}
  modifyMVar_ var onVar =
    ask >>= \e ->
      lift $ modifyMVar_ var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE modifyMVar_ #-}
  modifyMVar var onVar =
    ask >>= \e ->
      lift $ modifyMVar var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE modifyMVar #-}
  modifyMVarMasked_ var onVar =
    ask >>= \e ->
      lift $ modifyMVarMasked_ var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE modifyMVarMasked_ #-}
  modifyMVarMasked var onVar =
    ask >>= \e ->
      lift $ modifyMVarMasked var (\x -> runReaderT (onVar x) e)
  {-# INLINEABLE modifyMVarMasked #-}
  tryReadMVar = lift . tryReadMVar
  {-# INLINEABLE tryReadMVar #-}
  mkWeakMVar var m = ask >>= \e -> lift $ mkWeakMVar var (runReaderT m e)
  {-# INLINEABLE mkWeakMVar #-}

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
