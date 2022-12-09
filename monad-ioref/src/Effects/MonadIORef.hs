-- | Provides the 'MonadIORef' class.
--
-- @since 0.1
module Effects.MonadIORef
  ( -- * Class
    MonadIORef (..),

    -- * Reexports
    IORef,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Effects.MonadCallStack (addCallStack)
import GHC.Stack (HasCallStack)

-- | 'IORef' effect.
--
-- @since 0.1
class Monad m => MonadIORef m where
  -- | Creates a new 'IORef'.
  --
  -- @since 0.1
  newIORef :: HasCallStack => a -> m (IORef a)

  -- | Returns the 'IORef'.
  --
  -- @since 0.1
  readIORef :: HasCallStack => IORef a -> m a

  -- | Writes to an 'IORef'.
  --
  -- @since 0.1
  writeIORef :: HasCallStack => IORef a -> a -> m ()

  -- | Strictly modifies an 'IORef'.
  --
  -- @since 0.1
  modifyIORef' :: HasCallStack => IORef a -> (a -> a) -> m ()

-- | @since 0.1
instance MonadIORef IO where
  newIORef = addCallStack . IORef.newIORef
  readIORef = addCallStack . IORef.readIORef
  writeIORef r = addCallStack . IORef.writeIORef r
  modifyIORef' r = addCallStack . IORef.modifyIORef' r

-- | @since 0.1
instance MonadIORef m => MonadIORef (ReaderT e m) where
  newIORef = lift . newIORef
  readIORef = lift . readIORef
  writeIORef r = lift . writeIORef r
  modifyIORef' r = lift . modifyIORef' r
