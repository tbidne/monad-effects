-- | Provides the 'MonadIORef' class.
--
-- @since 0.1
module Effects.IORef
  ( -- * Effect
    MonadIORef (..),

    -- * Functions
    atomicModifyIORef'_,

    -- * Reexports
    IORef,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Effects.Exception (addCS)
import GHC.Stack (HasCallStack)

-- | 'IORef' effect.
--
-- @since 0.1
class (Monad m) => MonadIORef m where
  -- | Lifted 'IORef.newIORef'.
  --
  -- @since 0.1
  newIORef :: (HasCallStack) => a -> m (IORef a)

  -- | Lifted 'IORef.readIORef'.
  --
  -- @since 0.1
  readIORef :: (HasCallStack) => IORef a -> m a

  -- | Lifted 'IORef.writeIORef'.
  --
  -- @since 0.1
  writeIORef :: (HasCallStack) => IORef a -> a -> m ()

  -- | Lifted 'IORef.atomicWriteIORef'.
  --
  -- @since 0.1
  atomicWriteIORef :: (HasCallStack) => IORef a -> a -> m ()

  -- | Lifted 'IORef.modifyIORef''.
  --
  -- @since 0.1
  modifyIORef' :: (HasCallStack) => IORef a -> (a -> a) -> m ()

  -- | Lifted 'IORef.atomicModifyIORef''.
  --
  -- @since 0.1
  atomicModifyIORef' :: (HasCallStack) => IORef a -> (a -> (a, b)) -> m b

-- | @since 0.1
instance MonadIORef IO where
  newIORef = addCS . IORef.newIORef
  {-# INLINEABLE newIORef #-}
  readIORef = addCS . IORef.readIORef
  {-# INLINEABLE readIORef #-}
  writeIORef r = addCS . IORef.writeIORef r
  {-# INLINEABLE writeIORef #-}
  atomicWriteIORef r = addCS . IORef.atomicWriteIORef r
  {-# INLINEABLE atomicWriteIORef #-}
  modifyIORef' r = addCS . IORef.modifyIORef' r
  {-# INLINEABLE modifyIORef' #-}
  atomicModifyIORef' r = addCS . IORef.atomicModifyIORef' r
  {-# INLINEABLE atomicModifyIORef' #-}

-- | @since 0.1
instance (MonadIORef m) => MonadIORef (ReaderT e m) where
  newIORef = lift . newIORef
  {-# INLINEABLE newIORef #-}
  readIORef = lift . readIORef
  {-# INLINEABLE readIORef #-}
  writeIORef r = lift . writeIORef r
  {-# INLINEABLE writeIORef #-}
  atomicWriteIORef r = lift . atomicWriteIORef r
  {-# INLINEABLE atomicWriteIORef #-}
  modifyIORef' r = lift . modifyIORef' r
  {-# INLINEABLE modifyIORef' #-}
  atomicModifyIORef' r = lift . atomicModifyIORef' r
  {-# INLINEABLE atomicModifyIORef' #-}

-- | Atomically apply a function to the contents of an 'IORef' and return the
-- old and new values. The result of the function is forced.
--
-- @since 0.1
atomicModifyIORef'_ :: (HasCallStack, MonadIORef m) => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
{-# INLINEABLE atomicModifyIORef'_ #-}
