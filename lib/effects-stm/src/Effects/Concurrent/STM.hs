-- | Provides classes for 'STM'.
--
-- @since 0.1
module Effects.Concurrent.STM
  ( -- * Effect
    STM,
    MonadSTM (..),

    -- * TVar
    TVar,
    newTVarA,
    readTVarA,
    writeTVarA,
    modifyTVarA',

    -- * TBQueue
    TBQueue,
    newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
    flushTBQueueA,

    -- * Reexports
    Natural,
  )
where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Effects.Exception (addCS)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | 'STM' effect. Note that this class is for monads that can lift entire
-- STM transactions (i.e. atomically). It is not intended for "STM-like"
-- monads -- hence has no STM instance -- as the semantics for "STM-like" and
-- "can lift STM atomically" are different.
--
-- @since 0.1
class (Monad m) => MonadSTM m where
  -- | Lifted 'STM.atomically'.
  --
  -- @since 0.1
  atomically :: (HasCallStack) => STM a -> m a

-- | @since 0.1
instance MonadSTM IO where
  atomically = addCS . STM.atomically
  {-# INLINEABLE atomically #-}

-- | @since 0.1
instance (MonadSTM m) => MonadSTM (ReaderT e m) where
  atomically = lift . atomically
  {-# INLINEABLE atomically #-}

-- | Create a new 'TVar' holding a value supplied and lifts the result via
-- 'atomically'.
--
-- @since 0.1
newTVarA :: (HasCallStack, MonadSTM m) => a -> m (TVar a)
newTVarA = atomically . TVar.newTVar
{-# INLINEABLE newTVarA #-}

-- | Return the current value stored in a 'TVar' and lifts the result via
-- 'atomically'.
--
-- @since 0.1
readTVarA :: (HasCallStack, MonadSTM m) => TVar a -> m a
readTVarA = atomically . TVar.readTVar
{-# INLINEABLE readTVarA #-}

-- | Write the supplied value into a 'TVar' and lifts the action via
-- 'atomically'.
--
-- @since 0.1
writeTVarA :: (HasCallStack, MonadSTM m) => TVar a -> a -> m ()
writeTVarA r = atomically . TVar.writeTVar r
{-# INLINEABLE writeTVarA #-}

-- | Strict version of 'TVar.modifyTVar', lifting the action via
-- 'atomically'.
--
-- @since 0.1
modifyTVarA' :: (HasCallStack, MonadSTM m) => TVar a -> (a -> a) -> m ()
modifyTVarA' r = atomically . TVar.modifyTVar' r
{-# INLINEABLE modifyTVarA' #-}

-- | Builds and returns a new instance of 'TBQueue', lifting via 'atomically'.
--
-- @since 0.1
newTBQueueA ::
  (HasCallStack, MonadSTM m) =>
  -- | maximum number of elements the queue can hold
  Natural ->
  m (TBQueue a)
newTBQueueA = atomically . TBQueue.newTBQueue
{-# INLINEABLE newTBQueueA #-}

-- | Read the next value from the 'TBQueue', lifting via 'atomically'.
--
-- @since 0.1
readTBQueueA :: (HasCallStack, MonadSTM m) => TBQueue a -> m a
readTBQueueA = atomically . TBQueue.readTBQueue
{-# INLINEABLE readTBQueueA #-}

-- | A version of 'TBQueue.readTBQueue' which does not retry. Instead it
-- returns @Nothing@ if no value is available. Lifts via 'atomically'.
--
-- @since 0.1
tryReadTBQueueA :: (HasCallStack, MonadSTM m) => TBQueue a -> m (Maybe a)
tryReadTBQueueA = atomically . TBQueue.tryReadTBQueue
{-# INLINEABLE tryReadTBQueueA #-}

-- | Write a value to a 'TBQueue'; blocks if the queue is full. Lifts via
-- 'atomically'.
--
-- @since 0.1
writeTBQueueA :: (HasCallStack, MonadSTM m) => TBQueue a -> a -> m ()
writeTBQueueA q = atomically . TBQueue.writeTBQueue q
{-# INLINEABLE writeTBQueueA #-}

-- | Efficiently read the entire contents of a 'TBQueue' into a list. This
-- function never retries. Lifts via 'atomically'.
--
-- @since 0.1
flushTBQueueA :: (HasCallStack, MonadSTM m) => TBQueue a -> m [a]
flushTBQueueA = atomically . TBQueue.flushTBQueue
{-# INLINEABLE flushTBQueueA #-}
