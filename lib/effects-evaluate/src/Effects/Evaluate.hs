-- | Provides the 'MonadEvaluate' class.
--
-- @since 0.1
module Effects.Evaluate
  ( -- * Effect
    MonadEvaluate (..),
  )
where

import Control.Exception qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import GHC.Stack (HasCallStack)

-- | 'Evaluate' effect.
--
-- @since 0.1
class (Monad m) => MonadEvaluate m where
  -- | Lifted 'Ex.evaluate'.
  --
  -- @since 0.1
  evaluate :: (HasCallStack) => a -> m a

-- | @since 0.1
instance MonadEvaluate IO where
  evaluate = Ex.evaluate
  {-# INLINEABLE evaluate #-}

-- | @since 0.1
instance (MonadEvaluate m) => MonadEvaluate (ReaderT e m) where
  evaluate = lift . evaluate
  {-# INLINEABLE evaluate #-}
