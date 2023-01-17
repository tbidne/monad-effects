{-# LANGUAGE CPP #-}

-- | Provides the 'MonadEnv' typeclass.
--
-- @since 0.1
module Effects.MonadEnv
  ( -- * Effect
    MonadEnv (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Effects.MonadCallStack (addCallStack)
import GHC.Stack (HasCallStack)
import Prelude hiding (getChar, getLine, print, putStr, putStrLn)
import System.Environment qualified as Env

-- | Environment effects.
--
-- @since 0.1
class Monad m => MonadEnv m where
  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getArgs :: HasCallStack => m [String]

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getProgName :: HasCallStack => m String

#if MIN_VERSION_base(4,17,0)
  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  executablePath :: HasCallStack => Maybe (m (Maybe FilePath))
#endif

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getExecutablePath :: HasCallStack => m FilePath

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getEnv :: HasCallStack => String -> m String

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  lookupEnv :: HasCallStack => String -> m (Maybe String)

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  setEnv :: HasCallStack => String -> String -> m ()

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  unsetEnv :: HasCallStack => String -> m ()

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  withArgs :: HasCallStack => [String] -> m a -> m a

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  withProgName :: HasCallStack => String -> m a -> m a

  -- | Lifted 'Env.getArgs'.
  --
  -- @since 0.1
  getEnvironment :: HasCallStack => m [(String, String)]

-- | @since 0.1
instance MonadEnv IO where
  getArgs = addCallStack Env.getArgs
  {-# INLINEABLE getArgs #-}
  getProgName = addCallStack Env.getProgName
  {-# INLINEABLE getProgName #-}
#if MIN_VERSION_base(4,17,0)
  executablePath = fmap addCallStack Env.executablePath
  {-# INLINEABLE executablePath #-}
#endif
  getExecutablePath = addCallStack Env.getExecutablePath
  {-# INLINEABLE getExecutablePath #-}
  getEnv = addCallStack . Env.getEnv
  {-# INLINEABLE getEnv #-}
  lookupEnv = addCallStack . Env.lookupEnv
  {-# INLINEABLE lookupEnv #-}
  setEnv x = addCallStack . Env.setEnv x
  {-# INLINEABLE setEnv #-}
  unsetEnv = addCallStack . Env.unsetEnv
  {-# INLINEABLE unsetEnv #-}
  withArgs x = addCallStack . Env.withArgs x
  {-# INLINEABLE withArgs #-}
  withProgName x = addCallStack . Env.withProgName x
  {-# INLINEABLE withProgName #-}
  getEnvironment = addCallStack Env.getEnvironment
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
