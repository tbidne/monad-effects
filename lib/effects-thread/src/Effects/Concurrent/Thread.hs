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
import Control.Exception (Exception, evaluate)
import Control.Monad ((>=>))
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

-- NOTE: [Strictness design]
--
-- We want to provide strict (WHNF) versions of mutable functions to prevent
-- space leaks.
--
-- 1. First, there is a choice for evaluation:
--
--      a. Put strict functions in typeclass, use @evaluate :: a -> IO a@
--         in instance.
--
--      b. Use @(MonadEvaluate m) => evaluate :: a -> m a@ outside of typeclass.
--
--    A has the advantage that we do not need a MonadEvaluate dependency, and
--    we can simply reuse upstream definitions (e.g. modifyIORef') when they
--    exist.
--
--    B keeps the typeclass smaller. The main downside is that if upstream ever
--    adds a strict function -- which we should prefer over bespoke definitions
--    here -- then we could get in an awkward position where the lifted
--    upstream definition is in the typeclass, but other strict functions are
--    not (hence difference in MonadEvaluate constraint).
--
--    For that reason, and the fact that we may not be able to implement some
--    outside of the typeclass (e.g. hypothetical mkWeakMVar' requires IO), we
--    decide to put everything in the typeclass.
--
-- 2. Next, we have a choice of what WHNF actually means. At the very least
--    we want the values held by our mutable references to be in WHNF e.g.
--    the @a@ in @MVar a@.
--
--    But we have other choices too. Do we evaluate the output (@a@) of, say,
--    takeMVar? What about non-polymorphic values like @tryPutMVar@ returning
--    Bool? Finally, what about functions like @newEmptyMVar@ that have no
--    WHNF analog? Make a ticked alias or do nothing?
--
--    We take the principle that ticked functions should act as if the
--    underlying reference was strict e.g. (MVar a = MVar !a). Hence
--    we eval when writing and reading this value, but do not eval anything
--    else. We also do not include ticked aliases that do nothing.
--
--    See the following proposal for more details, and hopefully a more
--    robust plan in the future:
--
--      https://github.com/haskell/core-libraries-committee/issues/341

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

  -- | Evaluates the input to 'newMVar' to WHNF.
  --
  -- @since 0.1
  newMVar' :: a -> m (MVar a)

  -- | Lifted 'MVar.takeMVar'.
  --
  -- @since 0.1
  takeMVar :: MVar a -> m a

  -- | Evaluates the output of 'takeMVar' to WHNF.
  --
  -- @since 0.1
  takeMVar' :: MVar a -> m a

  -- | Lifted 'MVar.putMVar'.
  --
  -- @since 0.1
  putMVar :: MVar a -> a -> m ()

  -- | Evaluates the input to 'putMVar' to WHNF.
  --
  -- @since 0.1
  putMVar' :: MVar a -> a -> m ()

  -- | Lifted 'MVar.tryTakeMVar'.
  --
  -- @since 0.1
  tryTakeMVar :: MVar a -> m (Maybe a)

  -- | Evaluates the output of 'tryTakeMVar' to Nothing or @Just a@, where
  -- @a@ is in WHNF.
  --
  -- @since 0.1
  tryTakeMVar' :: MVar a -> m (Maybe a)

  -- | Lifted 'MVar.tryPutMVar'.
  --
  -- @since 0.1
  tryPutMVar :: MVar a -> a -> m Bool

  -- | Evaluates the input to 'tryPutMVar' to WHNF.
  --
  -- @since 0.1
  tryPutMVar' :: MVar a -> a -> m Bool

  -- | Lifted 'MVar.isEmptyMVar'.
  --
  -- @since 0.1
  isEmptyMVar :: MVar a -> m Bool

  -- | Lifted 'MVar.withMVar'.
  --
  -- @since 0.1
  withMVar :: MVar a -> (a -> m b) -> m b

  -- | Evaluates the input of the callback to 'withMVar' to WHNF.
  --
  -- @since 0.1
  withMVar' :: MVar a -> (a -> m b) -> m b

  -- | Lifted 'MVar.withMVarMasked'.
  --
  -- @since 0.1
  withMVarMasked :: MVar a -> (a -> m b) -> m b

  -- | Evaluates the input of the callback to 'withMVarMasked' to WHNF.
  --
  -- @since 0.1
  withMVarMasked' :: MVar a -> (a -> m b) -> m b

  -- | Lifted 'MVar.modifyMVar_'.
  --
  -- @since 0.1
  modifyMVar_ :: MVar a -> (a -> m a) -> m ()

  -- | Evaluates 'modifyMVar_'\'s modifier's input and output to WHNF.
  --
  -- @since 0.1
  modifyMVar_' :: MVar a -> (a -> m a) -> m ()

  -- | Lifted 'MVar.modifyMVar'.
  --
  -- @since 0.1
  modifyMVar :: MVar a -> (a -> m (a, b)) -> m b

  -- | Evaluates 'modifyMVar'\'s modifier's input and left-output to WHNF.
  --
  -- @since 0.1
  modifyMVar' :: MVar a -> (a -> m (a, b)) -> m b

  -- | Lifted 'MVar.modifyMVarMasked_'.
  --
  -- @since 0.1
  modifyMVarMasked_ :: MVar a -> (a -> m a) -> m ()

  -- | Evaluates 'modifyMVarMasked_'\'s modifier's input and output to WHNF.
  --
  -- @since 0.1
  modifyMVarMasked_' :: MVar a -> (a -> m a) -> m ()

  -- | Lifted 'MVar.modifyMVarMasked'.
  --
  -- @since 0.1
  modifyMVarMasked :: MVar a -> (a -> m (a, b)) -> m b

  -- | Evaluates 'modifyMVarMasked'\'s modifier's input and left-output to WHNF.
  --
  -- @since 0.1
  modifyMVarMasked' :: MVar a -> (a -> m (a, b)) -> m b

  -- | Lifted 'MVar.tryReadMVar'.
  --
  -- @since 0.1
  tryReadMVar :: MVar a -> m (Maybe a)

  -- | Evaluates the output of 'tryReadMVar' to Nothing or @Just a@, where
  -- @a@ is in WHNF.
  --
  -- @since 0.1
  tryReadMVar' :: MVar a -> m (Maybe a)

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

  newMVar' = evaluate >=> newMVar
  {-# INLINEABLE newMVar' #-}

  takeMVar = MVar.takeMVar
  {-# INLINEABLE takeMVar #-}

  takeMVar' = takeMVar >=> evaluate
  {-# INLINEABLE takeMVar' #-}

  putMVar = MVar.putMVar
  {-# INLINEABLE putMVar #-}

  putMVar' v = evaluate >=> putMVar v
  {-# INLINEABLE putMVar' #-}

  tryTakeMVar = MVar.tryTakeMVar
  {-# INLINEABLE tryTakeMVar #-}

  tryTakeMVar' v =
    tryTakeMVar v >>= \case
      Nothing -> pure Nothing
      Just x -> Just <$> evaluate x
  {-# INLINEABLE tryTakeMVar' #-}

  tryPutMVar = MVar.tryPutMVar
  {-# INLINEABLE tryPutMVar #-}

  tryPutMVar' v = evaluate >=> tryPutMVar v
  {-# INLINEABLE tryPutMVar' #-}

  isEmptyMVar = MVar.isEmptyMVar
  {-# INLINEABLE isEmptyMVar #-}

  withMVar = MVar.withMVar
  {-# INLINEABLE withMVar #-}

  withMVar' v f = withMVar v (evaluate >=> f)
  {-# INLINEABLE withMVar' #-}

  withMVarMasked = MVar.withMVarMasked
  {-# INLINEABLE withMVarMasked #-}

  withMVarMasked' v f = withMVarMasked v (evaluate >=> f)
  {-# INLINEABLE withMVarMasked' #-}

  modifyMVar_ = MVar.modifyMVar_
  {-# INLINEABLE modifyMVar_ #-}

  modifyMVar_' v f = modifyMVar_ v (evaluate >=> f >=> evaluate)
  {-# INLINEABLE modifyMVar_' #-}

  modifyMVar = MVar.modifyMVar
  {-# INLINEABLE modifyMVar #-}

  modifyMVar' v f = modifyMVar v $ \x -> do
    (a, b) <- f =<< evaluate x
    (,b) <$> evaluate a
  {-# INLINEABLE modifyMVar' #-}

  modifyMVarMasked_ = MVar.modifyMVarMasked_
  {-# INLINEABLE modifyMVarMasked_ #-}

  modifyMVarMasked_' v f = modifyMVarMasked_ v (evaluate >=> f >=> evaluate)
  {-# INLINEABLE modifyMVarMasked_' #-}

  modifyMVarMasked = MVar.modifyMVarMasked
  {-# INLINEABLE modifyMVarMasked #-}

  modifyMVarMasked' v f = modifyMVarMasked v $ \x -> do
    (a, b) <- f =<< evaluate x
    (,b) <$> evaluate a
  {-# INLINEABLE modifyMVarMasked' #-}

  tryReadMVar = MVar.tryReadMVar
  {-# INLINEABLE tryReadMVar #-}

  tryReadMVar' v =
    tryReadMVar v >>= \case
      Nothing -> pure Nothing
      Just x -> Just <$> evaluate x
  {-# INLINEABLE tryReadMVar' #-}

  mkWeakMVar = MVar.mkWeakMVar
  {-# INLINEABLE mkWeakMVar #-}

-- | @since 0.1
instance (MonadMVar m) => MonadMVar (ReaderT e m) where
  newEmptyMVar = lift newEmptyMVar
  {-# INLINEABLE newEmptyMVar #-}
  newMVar = lift . newMVar
  {-# INLINEABLE newMVar #-}
  newMVar' = lift . newMVar'
  {-# INLINEABLE newMVar' #-}
  takeMVar = lift . takeMVar
  {-# INLINEABLE takeMVar #-}
  takeMVar' = lift . takeMVar'
  {-# INLINEABLE takeMVar' #-}
  putMVar x1 = lift . putMVar x1
  {-# INLINEABLE putMVar #-}
  putMVar' x1 = lift . putMVar' x1
  {-# INLINEABLE putMVar' #-}
  tryTakeMVar = lift . tryTakeMVar
  {-# INLINEABLE tryTakeMVar #-}
  tryTakeMVar' = lift . tryTakeMVar'
  {-# INLINEABLE tryTakeMVar' #-}
  tryPutMVar x1 = lift . tryPutMVar x1
  {-# INLINEABLE tryPutMVar #-}
  tryPutMVar' x1 = lift . tryPutMVar' x1
  {-# INLINEABLE tryPutMVar' #-}
  isEmptyMVar = lift . isEmptyMVar
  {-# INLINEABLE isEmptyMVar #-}
  withMVar = runInReader . withMVar
  {-# INLINEABLE withMVar #-}
  withMVar' = runInReader . withMVar'
  {-# INLINEABLE withMVar' #-}
  withMVarMasked = runInReader . withMVarMasked
  {-# INLINEABLE withMVarMasked #-}
  withMVarMasked' = runInReader . withMVarMasked'
  {-# INLINEABLE withMVarMasked' #-}
  modifyMVar_ = runInReader . modifyMVar_
  {-# INLINEABLE modifyMVar_ #-}
  modifyMVar_' = runInReader . modifyMVar_'
  {-# INLINEABLE modifyMVar_' #-}
  modifyMVar = runInReader . modifyMVar
  {-# INLINEABLE modifyMVar #-}
  modifyMVar' = runInReader . modifyMVar'
  {-# INLINEABLE modifyMVar' #-}
  modifyMVarMasked_ = runInReader . modifyMVarMasked_
  {-# INLINEABLE modifyMVarMasked_ #-}
  modifyMVarMasked_' = runInReader . modifyMVarMasked_'
  {-# INLINEABLE modifyMVarMasked_' #-}
  modifyMVarMasked = runInReader . modifyMVarMasked
  {-# INLINEABLE modifyMVarMasked #-}
  modifyMVarMasked' = runInReader . modifyMVarMasked'
  {-# INLINEABLE modifyMVarMasked' #-}
  tryReadMVar = lift . tryReadMVar
  {-# INLINEABLE tryReadMVar #-}
  tryReadMVar' = lift . tryReadMVar'
  {-# INLINEABLE tryReadMVar' #-}
  mkWeakMVar var = runInReader (\g -> mkWeakMVar var (g ())) . const
  {-# INLINEABLE mkWeakMVar #-}

-- Morally @lift $ f $ \x -> unlift (g x)@.
runInReader :: (Monad m) => ((a -> m b) -> m c) -> (a -> ReaderT e m b) -> ReaderT e m c
runInReader f g = ask >>= \e -> lift (f (\x -> runReaderT (g x) e))

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
