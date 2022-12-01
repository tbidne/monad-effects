{-# LANGUAGE ImplicitParams #-}

-- | Provides an effect for handling exceptions with callstacks.
--
-- @since 0.1
module Effects.MonadCallStack
  ( -- * Class
    MonadCallStack (..),

    -- * Reexports
    Annotated.throw,
    Annotated.try,
    Annotated.catch,
  )
where

import Control.Exception.Annotated.UnliftIO (Exception)
import Control.Exception.Annotated.UnliftIO qualified as Annotated
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask)
import GHC.Stack (CallStack, HasCallStack)

-- | Typeclass for 'CallStack' effects. The 'IO' instance uses the machinery
-- from @annotated-exception@. Note that this means the try/catch/etc.
-- function from that package should be used, if you are using the 'IO'
-- instance.
--
-- @since 0.1
class Monad m => MonadCallStack m where
  -- | Retrieves the 'CallStack'.
  --
  -- @since 0.1
  getCallStack :: HasCallStack => m CallStack

  -- | Throws an exception with the 'CallStack'.
  --
  -- @since 0.1
  throwWithCallStack :: (Exception e, HasCallStack) => e -> m a

  -- | Adds the 'CallStack' to any thrown exceptions.
  --
  -- @since 0.1
  checkpointCallStack :: HasCallStack => m a -> m a

-- | @since 0.1
instance MonadCallStack IO where
  getCallStack = pure ?callStack
  throwWithCallStack = Annotated.throwWithCallStack
  checkpointCallStack = Annotated.checkpointCallStack

-- | @since 0.1
instance MonadCallStack m => MonadCallStack (ReaderT e m) where
  getCallStack = lift getCallStack
  throwWithCallStack = lift . throwWithCallStack
  checkpointCallStack (ReaderT r) = ask >>= lift . checkpointCallStack . r
