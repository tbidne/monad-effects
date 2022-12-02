-- | Provides classes for 'STM'.
--
-- @since 0.1
module Effects.MonadSTM
  ( -- * Classes
    MonadSTM (..),
    MonadTVar (..),
    MonadTBQueue (..),

    -- * Reexports
    STM,
    TVar,
    TBQueue,
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
import Effects.MonadCallStack (checkpointCallStack)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | 'STM' effect.
--
-- @since 0.1
class Monad m => MonadSTM m where
  -- | Returns the local system time.
  --
  -- @since 0.1
  atomically :: HasCallStack => STM a -> m a

-- | @since 0.1
instance MonadSTM IO where
  atomically = checkpointCallStack . STM.atomically

-- | @since 0.1
instance MonadSTM m => MonadSTM (ReaderT e m) where
  atomically = lift . atomically

-- | 'TVar' effect.
--
-- @since 0.1
class Monad m => MonadTVar m where
  -- | Creates a new 'TVar' in @m@.
  --
  -- @since 0.1
  newTVarM :: a -> m (TVar a)

  -- | Reads a 'TVar' in @m@.
  --
  -- @since 0.1
  readTVarM :: TVar a -> m a

  -- | Writes to a 'TVar' in @m@.
  --
  -- @since 0.1
  writeTVarM :: TVar a -> a -> m ()

  -- | Strictly Modifies a 'TVar' in @m@.
  --
  -- @since 0.1
  modifyTVarM' :: TVar a -> (a -> a) -> m ()

-- | @since 0.1
instance MonadTVar IO where
  newTVarM = checkpointCallStack . STM.atomically . TVar.newTVar
  readTVarM = checkpointCallStack . STM.atomically . TVar.readTVar
  writeTVarM r = checkpointCallStack . STM.atomically . TVar.writeTVar r
  modifyTVarM' r = checkpointCallStack . STM.atomically . TVar.modifyTVar' r

-- | @since 0.1
instance MonadTVar m => MonadTVar (ReaderT e m) where
  newTVarM = lift . newTVarM
  readTVarM = lift . readTVarM
  writeTVarM r = lift . writeTVarM r
  modifyTVarM' r = lift . modifyTVarM' r

-- | 'TVar' effect.
--
-- @since 0.1
class Monad m => MonadTBQueue m where
  -- | Creates a new 'TBQueue' in @m@.
  --
  -- @since 0.1
  newTBQueueM :: Natural -> m (TBQueue a)

  -- | Reads a 'TBQueue' in @m@.
  --
  -- @since 0.1
  readTBQueueM :: TBQueue a -> m a

  -- | Reads a 'TBQueue' in @m@.
  --
  -- @since 0.1
  tryReadTBQueueM :: TBQueue a -> m (Maybe a)

  -- | Writes to a 'TBQueue' in @m@.
  --
  -- @since 0.1
  writeTBQueueM :: TBQueue a -> a -> m ()

  -- | Strictly Modifies a 'TBQueue' in @m@.
  --
  -- @since 0.1
  flushTBQueueM :: TBQueue a -> m [a]

  -- | @since 0.1

instance MonadTBQueue IO where
  newTBQueueM = checkpointCallStack . STM.atomically . TBQueue.newTBQueue
  readTBQueueM = checkpointCallStack . STM.atomically . TBQueue.readTBQueue
  tryReadTBQueueM = checkpointCallStack . STM.atomically . TBQueue.tryReadTBQueue
  writeTBQueueM q = checkpointCallStack . STM.atomically . TBQueue.writeTBQueue q
  flushTBQueueM = checkpointCallStack . STM.atomically . TBQueue.flushTBQueue

-- \| @since 0.1
instance MonadTBQueue m => MonadTBQueue (ReaderT e m) where
  newTBQueueM = lift . newTBQueueM
  readTBQueueM = lift . readTBQueueM
  tryReadTBQueueM = lift . tryReadTBQueueM
  writeTBQueueM q = lift . writeTBQueueM q
  flushTBQueueM = lift . flushTBQueueM
