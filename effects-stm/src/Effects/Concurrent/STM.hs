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

-- | 'STM' effect.
--
-- @since 0.1
class (Monad m) => MonadSTM m where
  -- | Perform a series of STM actions atomically.
  --
  -- Using 'atomically' inside an 'unsafePerformIO' or 'unsafeInterleaveIO'
  -- subverts some of guarantees that STM provides. It makes it possible to
  -- run a transaction inside of another transaction, depending on when the
  -- thunk is evaluated. If a nested transaction is attempted, an exception
  -- is thrown by the runtime. It is possible to safely use 'atomically' inside
  -- 'unsafePerformIO' or 'unsafeInterleaveIO', but the typechecker does not
  -- rule out programs that may attempt nested transactions, meaning that
  -- the programmer must take special care to prevent these.
  --
  -- However, there are functions for creating transactional variables that
  -- can always be safely called in 'unsafePerformIO'. See: 'newTVarIO',
  -- 'Control.Concurrent.STM.TChan.newTChanIO',
  -- 'Control.Concurrent.STM.TChan.newBroadcastTChanIO',
  -- 'Control.Concurrent.STM.TQueue.newTQueueIO',
  -- 'Control.Concurrent.STM.TBQueue.newTBQueueIO', and
  -- 'Control.Concurrent.STM.TMVar.newTMVarIO'.
  --
  -- Using 'unsafePerformIO' inside of 'atomically' is also dangerous but for
  -- different reasons. See 'unsafeIOToSTM' for more on this.
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

-- | A version of 'readTBQueue' which does not retry. Instead it
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
