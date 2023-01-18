-- | Provides classes for 'STM'.
--
-- @since 0.1
module Effects.Concurrent.MonadSTM
  ( -- * Effect
    STM,
    MonadSTM (..),

    -- * TVar
    TVar,
    newTVarM,
    readTVarM,
    writeTVarM,
    modifyTVarM',

    -- * TBQueue
    TBQueue,
    newTBQueueM,
    readTBQueueM,
    tryReadTBQueueM,
    writeTBQueueM,
    flushTBQueueM,

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
import Effects.MonadCallStack (addCallStack)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)

-- | 'STM' effect.
--
-- @since 0.1
class Monad m => MonadSTM m where
  -- | Lifted 'STM.atomically'.
  --
  -- @since 0.1
  atomically :: HasCallStack => STM a -> m a

-- | @since 0.1
instance MonadSTM IO where
  atomically = addCallStack . STM.atomically
  {-# INLINEABLE atomically #-}

-- | @since 0.1
instance MonadSTM m => MonadSTM (ReaderT e m) where
  atomically = lift . atomically
  {-# INLINEABLE atomically #-}

-- | Lifted 'TVar.newTVar'.
--
-- @since 0.1
newTVarM :: (HasCallStack, MonadSTM m) => a -> m (TVar a)
newTVarM = atomically . TVar.newTVar
{-# INLINEABLE newTVarM #-}

-- | Lifted 'TVar.readTVar'.
--
-- @since 0.1
readTVarM :: (HasCallStack, MonadSTM m) => TVar a -> m a
readTVarM = atomically . TVar.readTVar
{-# INLINEABLE readTVarM #-}

-- | Lifted 'TVar.writeTVar'.
--
-- @since 0.1
writeTVarM :: (HasCallStack, MonadSTM m) => TVar a -> a -> m ()
writeTVarM r = atomically . TVar.writeTVar r
{-# INLINEABLE writeTVarM #-}

-- | Lifted 'TVar.modifyTVar''.
--
-- @since 0.1
modifyTVarM' :: (HasCallStack, MonadSTM m) => TVar a -> (a -> a) -> m ()
modifyTVarM' r = atomically . TVar.modifyTVar' r
{-# INLINEABLE modifyTVarM' #-}

-- | Lifted 'TBQueue.newTBQueue'.
--
-- @since 0.1
newTBQueueM :: (HasCallStack, MonadSTM m) => Natural -> m (TBQueue a)
newTBQueueM = atomically . TBQueue.newTBQueue
{-# INLINEABLE newTBQueueM #-}

-- | Lifted 'TBQueue.readTBQueue'.
--
-- @since 0.1
readTBQueueM :: (HasCallStack, MonadSTM m) => TBQueue a -> m a
readTBQueueM = atomically . TBQueue.readTBQueue
{-# INLINEABLE readTBQueueM #-}

-- | Lifted 'TBQueue.tryReadTBQueue'.
--
-- @since 0.1
tryReadTBQueueM :: (HasCallStack, MonadSTM m) => TBQueue a -> m (Maybe a)
tryReadTBQueueM = atomically . TBQueue.tryReadTBQueue
{-# INLINEABLE tryReadTBQueueM #-}

-- | Lifted 'TBQueue.writeTBQueue'.
--
-- @since 0.1
writeTBQueueM :: (HasCallStack, MonadSTM m) => TBQueue a -> a -> m ()
writeTBQueueM q = atomically . TBQueue.writeTBQueue q
{-# INLINEABLE writeTBQueueM #-}

-- | Lifted 'TBQueue.flushTBQueue'.
--
-- @since 0.1
flushTBQueueM :: (HasCallStack, MonadSTM m) => TBQueue a -> m [a]
flushTBQueueM = atomically . TBQueue.flushTBQueue
{-# INLINEABLE flushTBQueueM #-}
