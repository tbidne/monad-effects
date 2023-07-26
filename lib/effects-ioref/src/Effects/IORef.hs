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

-- | 'IORef' effect.
--
-- @since 0.1
class (Monad m) => MonadIORef m where
  -- | Build a new 'IORef'.
  --
  -- @since 0.1
  newIORef :: a -> m (IORef a)

  -- | Read the value of an 'IORef'.
  --
  -- @since 0.1
  readIORef :: IORef a -> m a

  -- | Write a new value into an 'IORef'.
  --
  -- @since 0.1
  writeIORef :: IORef a -> a -> m ()

  -- | Variant of 'writeIORef' with the \"barrier to reordering\" property that
  -- 'atomicModifyIORef' has.
  --
  -- @since 0.1
  atomicWriteIORef :: IORef a -> a -> m ()

  -- | Strict version of 'modifyIORef'.
  --
  -- @since 0.1
  modifyIORef' :: IORef a -> (a -> a) -> m ()

  -- | Strict version of 'Data.IORef.atomicModifyIORef'. This forces both
  -- the value stored in the 'IORef' and the value returned. The new value
  -- is installed in the 'IORef' before the returned value is forced.
  -- So
  --
  -- @atomicModifyIORef' ref (\x -> (x+1, undefined))@
  --
  -- will increment the 'IORef' and then throw an exception in the calling
  -- thread.
  --
  -- @since 0.1
  atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> m b

-- | @since 0.1
instance MonadIORef IO where
  newIORef = IORef.newIORef
  {-# INLINEABLE newIORef #-}
  readIORef = IORef.readIORef
  {-# INLINEABLE readIORef #-}
  {-# INLINEABLE writeIORef #-}
  {-# INLINEABLE atomicWriteIORef #-}
  {-# INLINEABLE modifyIORef' #-}
  {-# INLINEABLE atomicModifyIORef' #-}
  writeIORef = IORef.writeIORef
  atomicWriteIORef = IORef.atomicWriteIORef
  modifyIORef' = IORef.modifyIORef'
  atomicModifyIORef' = IORef.atomicModifyIORef'

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
atomicModifyIORef'_ :: (MonadIORef m) => IORef a -> (a -> a) -> m ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \a -> (f a, ())
{-# INLINEABLE atomicModifyIORef'_ #-}
