{-# LANGUAGE MagicHash #-}

-- | Provides classes for 'STM'.
--
-- @since 0.1
module Effects.Concurrent.STM
  ( -- * Effect
    STM,
    MonadAtomic (..),

    -- * TVar
    TVar,

    -- ** Strict
    newTVar',
    readTVar',
    writeTVar',
    TVar.modifyTVar',

    -- *** Atomic
    newTVarA',
    readTVarA',
    writeTVarA',
    modifyTVarA',

    -- ** Lazy
    TVar.newTVar,
    TVar.readTVar,
    TVar.writeTVar,
    TVar.modifyTVar,

    -- *** Atomic
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA,

    -- * TBQueue
    TBQueue,

    -- ** Strict
    readTBQueue',
    tryReadTBQueue',
    writeTBQueue',
    flushTBQueue',

    -- *** Atomic
    readTBQueueA',
    tryReadTBQueueA',
    writeTBQueueA',
    flushTBQueueA',

    -- ** Lazy
    TBQueue.newTBQueue,
    TBQueue.readTBQueue,
    TBQueue.tryReadTBQueue,
    TBQueue.writeTBQueue,
    TBQueue.flushTBQueue,

    -- *** Atomic
    newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Reexports
    Natural,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import GHC.Conc (STM (STM))
import GHC.Exts (seq#)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | Effect for atomically lifting 'STM' actions. Note that this classes is
-- intended for "IO-like" monads /not/ "STM-like" monads -- hence has no STM
-- instance -- as the semantics for "STM-like" and "can lift STM atomically"
-- are different.
--
-- @since 0.1
class (Monad m) => MonadAtomic m where
  -- | Lifted 'STM.atomically'.
  --
  -- @since 0.1
  atomically :: (HasCallStack) => STM a -> m a

-- | @since 0.1
instance MonadAtomic IO where
  atomically = STM.atomically
  {-# INLINEABLE atomically #-}

-- | @since 0.1
instance (MonadAtomic m) => MonadAtomic (ReaderT e m) where
  atomically = lift . atomically
  {-# INLINEABLE atomically #-}

-- | Evaluates the input to 'TVar.newTVar' to WHNF.
--
-- @since 0.1
newTVar' :: a -> STM (TVar a)
newTVar' = evaluateSTM >=> TVar.newTVar

-- | Create a new 'TVar' holding a value supplied and lifts the result via
-- 'atomically'.
--
-- @since 0.1
newTVarA :: (HasCallStack, MonadAtomic m) => a -> m (TVar a)
newTVarA = atomically . TVar.newTVar
{-# INLINEABLE newTVarA #-}

-- | Atomic 'newTVar''.
--
-- @since 0.1
newTVarA' :: (HasCallStack, MonadAtomic m) => a -> m (TVar a)
newTVarA' = atomically . newTVar'
{-# INLINEABLE newTVarA' #-}

-- | Evaluates the output from 'TVar.readTVar' to WHNF.
--
-- @since 0.1
readTVar' :: TVar a -> STM a
readTVar' = TVar.readTVar >=> evaluateSTM

-- | Return the current value stored in a 'TVar' and lifts the result via
-- 'atomically'.
--
-- @since 0.1
readTVarA :: (HasCallStack, MonadAtomic m) => TVar a -> m a
readTVarA = atomically . TVar.readTVar
{-# INLINEABLE readTVarA #-}

-- | Atomic 'readTVarA''.
--
-- @since 0.1
readTVarA' :: (HasCallStack, MonadAtomic m) => TVar a -> m a
readTVarA' = atomically . readTVar'
{-# INLINEABLE readTVarA' #-}

-- | Evaluates the input to 'TVar.writeTVar' to WHNF.
--
-- @since 0.1
writeTVar' :: TVar a -> a -> STM ()
writeTVar' var = evaluateSTM >=> TVar.writeTVar var

-- | Write the supplied value into a 'TVar' and lifts the action via
-- 'atomically'.
--
-- @since 0.1
writeTVarA :: (HasCallStack, MonadAtomic m) => TVar a -> a -> m ()
writeTVarA r = atomically . TVar.writeTVar r
{-# INLINEABLE writeTVarA #-}

-- | Atomic 'writeTVarA''.
--
-- @since 0.1
writeTVarA' :: (HasCallStack, MonadAtomic m) => TVar a -> a -> m ()
writeTVarA' r = atomically . writeTVar' r
{-# INLINEABLE writeTVarA' #-}

-- | Atomic 'TVar.modifyTVar'.
--
-- @since 0.1
modifyTVarA :: (HasCallStack, MonadAtomic m) => TVar a -> (a -> a) -> m ()
modifyTVarA r = atomically . TVar.modifyTVar r
{-# INLINEABLE modifyTVarA #-}

-- | Atomic 'TVar.modifyTVar''.
--
-- @since 0.1
modifyTVarA' :: (HasCallStack, MonadAtomic m) => TVar a -> (a -> a) -> m ()
modifyTVarA' r = atomically . TVar.modifyTVar' r
{-# INLINEABLE modifyTVarA' #-}

-- | Builds and returns a new instance of 'TBQueue', lifting via 'atomically'.
--
-- @since 0.1
newTBQueueA ::
  (HasCallStack, MonadAtomic m) =>
  -- | maximum number of elements the queue can hold
  Natural ->
  m (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue
{-# INLINEABLE newTBQueueA #-}

-- | Evaluates the output from 'TBQueue.readTBQueue' to WHNF.
--
-- @since 0.1
readTBQueue' :: TBQueue a -> STM a
readTBQueue' = TBQueue.readTBQueue >=> evaluateSTM
{-# INLINEABLE readTBQueue' #-}

-- | Read the next value from the 'TBQueue', lifting via 'atomically'.
--
-- @since 0.1
readTBQueueA :: (HasCallStack, MonadAtomic m) => TBQueue a -> m a
readTBQueueA = atomically . TBQueue.readTBQueue
{-# INLINEABLE readTBQueueA #-}

-- | Atomic 'readTBQueue''.
--
-- @since 0.1
readTBQueueA' :: (HasCallStack, MonadAtomic m) => TBQueue a -> m a
readTBQueueA' = atomically . readTBQueue'
{-# INLINEABLE readTBQueueA' #-}

-- | Evaluates the output from 'TBQueue.tryReadTBQueue' to Nothing or
-- @Just a@, where @a@ is in WHNF.
--
-- @since 0.1
tryReadTBQueue' :: TBQueue a -> STM (Maybe a)
tryReadTBQueue' =
  TBQueue.tryReadTBQueue >=> \case
    Nothing -> pure Nothing
    Just x -> Just <$> evaluateSTM x
{-# INLINEABLE tryReadTBQueue' #-}

-- | A version of 'TBQueue.readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available. Lifts via 'atomically'.
--
-- @since 0.1
tryReadTBQueueA :: (HasCallStack, MonadAtomic m) => TBQueue a -> m (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue
{-# INLINEABLE tryReadTBQueueA #-}

-- | Atomic 'tryReadTBQueue''.
--
-- @since 0.1
tryReadTBQueueA' :: (HasCallStack, MonadAtomic m) => TBQueue a -> m (Maybe a)
tryReadTBQueueA' = atomically . tryReadTBQueue'
{-# INLINEABLE tryReadTBQueueA' #-}

-- | Evaluates the input to 'TBQueue.writeTBQueue' to WHNF.
--
-- @since 0.1
writeTBQueue' :: TBQueue a -> a -> STM ()
writeTBQueue' q = evaluateSTM >=> TBQueue.writeTBQueue q

-- | Write a value to a 'TBQueue'; blocks if the queue is full. Lifts via
-- 'atomically'.
--
-- @since 0.1
writeTBQueueA :: (HasCallStack, MonadAtomic m) => TBQueue a -> a -> m ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q
{-# INLINEABLE writeTBQueueA #-}

-- | Atomic 'writeTBQueue''.
--
-- @since 0.1
writeTBQueueA' :: (HasCallStack, MonadAtomic m) => TBQueue a -> a -> m ()
writeTBQueueA' q = atomically . writeTBQueue' q
{-# INLINEABLE writeTBQueueA' #-}

-- | Evaluates the output from 'TBQueue.flushTBQueue' to WHNF.
--
-- @since 0.1
flushTBQueue' :: TBQueue a -> STM [a]
flushTBQueue' = TBQueue.flushTBQueue >=> evaluateSTM

-- | Efficiently read the entire contents of a 'TBQueue' into a list. This
-- function never retries. Lifts via 'atomically'.
--
-- @since 0.1
flushTBQueueA :: (HasCallStack, MonadAtomic m) => TBQueue a -> m [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue
{-# INLINEABLE flushTBQueueA #-}

-- | Atomic 'flushTBQueue''.
--
-- @since 0.1
flushTBQueueA' :: (HasCallStack, MonadAtomic m) => TBQueue a -> m [a]
flushTBQueueA' = atomically . flushTBQueue'
{-# INLINEABLE flushTBQueueA' #-}

-- | Like 'Control.Exception.evaluate', but for 'STM'.
--
-- @since 0.1
evaluateSTM :: a -> STM a
evaluateSTM a = STM $ \s -> seq# a s
