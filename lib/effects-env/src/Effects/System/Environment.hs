{-# LANGUAGE CPP #-}

-- | Provides the 'MonadEnv' typeclass.
--
-- @since 0.1
module Effects.System.Environment
  ( -- * Effect
    MonadEnv (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import GHC.Stack (HasCallStack)
import System.Environment qualified as Env

{- ORMOLU_DISABLE -}

-- | Environment effects.
--
-- @since 0.1
class Monad m => MonadEnv m where
  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getArgs :: HasCallStack => m [String]

  -- | Lifted 'Env.getProgName'.
  --
  -- @since 0.1
  getProgName :: HasCallStack => m String

#if MIN_VERSION_base(4,17,0)
  -- | Lifted 'Env.executablePath'.
  --
  -- @since 0.1
  executablePath :: HasCallStack => Maybe (m (Maybe FilePath))
#endif

  -- | Lifted 'Env.getExecutablePath'.
  --
  -- @since 0.1
  getExecutablePath :: HasCallStack => m FilePath
  -- | Lifted 'Env.getEnv'.
  -- @since 0.1
  getEnv :: HasCallStack => String -> m String
  -- | Lifted 'Env.lookupEnv'.
  --
  -- @since 0.1
  lookupEnv :: HasCallStack => String -> m (Maybe String)
  -- | Lifted 'Env.setEnv'.
  --
  -- @since 0.1
  setEnv :: HasCallStack => String -> String -> m ()
  -- | Lifted 'Env.unsetEnv'.
  --
  -- @since 0.1
  unsetEnv :: HasCallStack => String -> m ()
  -- | Lifted 'Env.withArgs'.
  --
  -- @since 0.1
  withArgs :: HasCallStack => [String] -> m a -> m a
  -- | Lifted 'Env.withProgName'.
  --
  -- @since 0.1
  withProgName :: HasCallStack => String -> m a -> m a
  -- | Lifted 'Env.getEnvironment'.
  --
  -- @since 0.1
  getEnvironment :: HasCallStack => m [(String, String)]

-- | @since 0.1
instance MonadEnv IO where
  getArgs = Env.getArgs
  {-# INLINEABLE getArgs #-}
  getProgName = Env.getProgName
  {-# INLINEABLE getProgName #-}
#if MIN_VERSION_base(4,17,0)
  executablePath = Env.executablePath
  {-# INLINEABLE executablePath #-}
#endif
  getExecutablePath = Env.getExecutablePath
  {-# INLINEABLE getExecutablePath #-}
  getEnv = Env.getEnv
  {-# INLINEABLE getEnv #-}
  lookupEnv = Env.lookupEnv
  {-# INLINEABLE lookupEnv #-}
  setEnv = Env.setEnv
  {-# INLINEABLE setEnv #-}
  unsetEnv = Env.unsetEnv
  {-# INLINEABLE unsetEnv #-}
  withArgs = Env.withArgs
  {-# INLINEABLE withArgs #-}
  withProgName = Env.withProgName
  {-# INLINEABLE withProgName #-}
  getEnvironment = Env.getEnvironment
  {-# INLINEABLE getEnvironment #-}

-- | @since 0.1
instance MonadEnv m => MonadEnv (ReaderT env m) where
  getArgs = lift getArgs
  {-# INLINEABLE getArgs #-}
  getProgName = lift getProgName
  {-# INLINEABLE getProgName #-}
#if MIN_VERSION_base(4,17,0)
  executablePath = fmap lift executablePath
  {-# INLINEABLE executablePath #-}
#endif
  getExecutablePath = lift getExecutablePath
  {-# INLINEABLE getExecutablePath #-}
  getEnv = lift . getEnv
  {-# INLINEABLE getEnv #-}
  lookupEnv = lift . lookupEnv
  {-# INLINEABLE lookupEnv #-}
  setEnv x = lift . setEnv x
  {-# INLINEABLE setEnv #-}
  unsetEnv = lift . unsetEnv
  {-# INLINEABLE unsetEnv #-}
  withArgs x m = ask >>= \e -> lift (withArgs x (runReaderT m e))
  {-# INLINEABLE withArgs #-}
  withProgName x m = ask >>= \e -> lift (withProgName x (runReaderT m e))
  {-# INLINEABLE withProgName #-}
  getEnvironment = lift getEnvironment
  {-# INLINEABLE getEnvironment #-}

{- ORMOLU_ENABLE -}
