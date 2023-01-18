-- | Provides the 'MonadEnv' typeclass.
--
-- @since 0.1
module Effects.System.MonadExit
  ( -- * Effect
    MonadEnv (..),
    ExitCode (..),

    -- ** Functions
    exitFailure,
    exitSuccess,
    die,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString.Char8 qualified as Char8
import Effects.FileSystem.MonadHandleWriter
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode (..))
import System.Exit qualified as Exit
import System.IO qualified as IO

-- | Environment effects.
--
-- @since 0.1
class Monad m => MonadEnv m where
  -- | Lifted 'Env.exitWith'.
  --
  -- @since 0.1
  exitWith :: HasCallStack => ExitCode -> m a

-- | @since 0.1
instance MonadEnv IO where
  exitWith = Exit.exitWith
  {-# INLINEABLE exitWith #-}

-- | @since 0.1
instance MonadEnv m => MonadEnv (ReaderT env m) where
  exitWith = lift . exitWith
  {-# INLINEABLE exitWith #-}

-- | Lifted 'Exit.exitFailure'.
--
-- @since 0.1
exitFailure :: MonadEnv m => m a
exitFailure = exitWith (ExitFailure 1)
{-# INLINEABLE exitFailure #-}

-- | Lifted 'Exit.exitSuccess'.
--
-- @since 0.1
exitSuccess :: MonadEnv m => m a
exitSuccess = exitWith ExitSuccess
{-# INLINEABLE exitSuccess #-}

-- | Lifted 'die'.
--
-- @since 0.1
die :: (MonadEnv m, MonadHandleWriter m) => String -> m a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
