-- | Provides the 'MonadHaskeline' typeclass.
--
-- @since 0.1
module Effects.Haskeline
  ( -- * Class
    MonadHaskeline (..),

    -- * ReaderT
    -- $readert

    -- * Runners
    runInputTEnv,
    runInputTEnvWith,

    -- * Haskeline Re-exports

    -- ** Types
    InputT,
    InputTEnv,

    -- ** IO Runners
    H.runInputT,
    H.runInputTBehavior,
    H.runInputTBehaviorWithPrefs,

    -- ** Config
    H.defaultSettings,
    H.defaultBehavior,
    H.defaultPrefs,
  )
where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (ReaderT), runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.Trans (MonadTrans (lift))
import GHC.Stack (HasCallStack)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.History (History)
import System.Console.Haskeline.ReaderT (InputTEnv)
import System.Console.Haskeline.ReaderT qualified as HR

-- NOTE: We implement most of the effectful functions from
-- System.Console.Haskeline. What is missing is functions of the form
-- 'foo :: Input m a -> m a' i.e. they run the input to produce a result in the
-- underlying monad.
--
-- Ultimately, such functions like
--
--   runInputT :: Settings m -> InputT m a -> m a
--
-- do not make sense to mock anyway, since custom monads will implement
-- their own functions, and "real" code will call the real runInputT.

-- | Allows haskeline effects.
--
-- @since 0.1
class (Monad m) => MonadHaskeline m where
  -- | Lifted 'H.haveTerminalUI'.
  --
  -- @since 0.1
  haveTerminalUI :: (HasCallStack) => m Bool

  -- | Lifted 'H.getInputLine'.
  --
  -- @since 0.1
  getInputLine :: (HasCallStack) => String -> m (Maybe String)

  -- | Lifted 'H.getInputLineWithInitial'.
  --
  -- @since 0.1
  getInputLineWithInitial :: (HasCallStack) => String -> (String, String) -> m (Maybe String)

  -- | Lifted 'H.getInputChar'.
  --
  -- @since 0.1
  getInputChar :: (HasCallStack) => String -> m (Maybe Char)

  -- | Lifted 'H.getPassword'.
  --
  -- @since 0.1
  getPassword :: (HasCallStack) => Maybe Char -> String -> m (Maybe String)

  -- | Lifted 'H.waitForAnyKey'.
  --
  -- @since 0.1
  waitForAnyKey :: (HasCallStack) => String -> m Bool

  -- | Lifted 'H.outputStr'.
  --
  -- @since 0.1
  outputStr :: (HasCallStack) => String -> m ()

  -- | Lifted 'H.outputStrLn'.
  --
  -- @since 0.1
  outputStrLn :: (HasCallStack) => String -> m ()

  -- | Lifted 'H.getExternalPrint'.
  --
  -- @since 0.1
  getExternalPrint :: (HasCallStack) => m (String -> IO ())

  -- | Lifted 'H.getHistory'.
  --
  -- @since 0.1
  getHistory :: (HasCallStack) => m History

  -- | Lifted 'H.putHistory'.
  --
  -- @since 0.1
  putHistory :: (HasCallStack) => History -> m ()

  -- | Lifted 'H.modifyHistory'.
  --
  -- @since 0.1
  modifyHistory :: (HasCallStack) => (History -> History) -> m ()

  -- | Lifted 'H.withInterrupt'.
  --
  -- @since 0.1
  withInterrupt :: (HasCallStack) => m a -> m a

  -- | Lifted 'H.handleInterrupt'.
  --
  -- @since 0.1
  handleInterrupt :: (HasCallStack) => m a -> m a -> m a

-- | @since 0.1
instance (MonadIO m, MonadMask m) => MonadHaskeline (InputT m) where
  haveTerminalUI = H.haveTerminalUI
  {-# INLINEABLE haveTerminalUI #-}
  getInputLine = H.getInputLine
  {-# INLINEABLE getInputLine #-}
  getInputLineWithInitial = H.getInputLineWithInitial
  {-# INLINEABLE getInputLineWithInitial #-}
  getInputChar = H.getInputChar
  {-# INLINEABLE getInputChar #-}
  getPassword = H.getPassword
  {-# INLINEABLE getPassword #-}
  waitForAnyKey = H.waitForAnyKey
  {-# INLINEABLE waitForAnyKey #-}
  outputStr = H.outputStr
  {-# INLINEABLE outputStr #-}
  outputStrLn = H.outputStrLn
  {-# INLINEABLE outputStrLn #-}
  getExternalPrint = H.getExternalPrint
  {-# INLINEABLE getExternalPrint #-}
  getHistory = H.getHistory
  {-# INLINEABLE getHistory #-}
  putHistory = H.putHistory
  {-# INLINEABLE putHistory #-}
  modifyHistory = H.modifyHistory
  {-# INLINEABLE modifyHistory #-}
  withInterrupt = H.withInterrupt
  {-# INLINEABLE withInterrupt #-}
  handleInterrupt = H.handleInterrupt
  {-# INLINEABLE handleInterrupt #-}

-- $readert
--
-- We provide two (overlapping) 'Control.Monad.Reader.ReaderT' instances:
-- A standard instance i.e.
--
-- @
--   instance ('MonadHaskeline' m) => 'MonadHaskeline' ('Control.Monad.Reader.ReaderT.ReaderT' e m)
-- @
--
-- And one in terms of the concrete (abstract) haskeline environment:
--
-- @
--   instance 'MonadHaskeline' ('Control.Monad.Reader.ReaderT.ReaderT' ('InputTEnv' m) m)
-- @
--
-- The latter is the intended way to run the application in real code:
--
-- @
--   run :: ('MonadHaskeline' m, ...) => m ()
--
--   -- Uses 'Control.Monad.Reader.ReaderT.ReaderT' ('InputTEnv' m) instance
--   main :: IO ()
--   main = 'runInputTDefault' 'runReaderT' run
-- @
--
-- On the other hand, the former is useful for deriving e.g.
--
-- @
--   newtype AppT e m a = MkAppT {unAppT :: 'Control.Monad.Reader.ReaderT.ReaderT' e m a }
--     deriving ('Applicative', 'Functor', 'Monad') via ('Control.Monad.Reader.ReaderT.ReaderT' e m)
--
--   deriving instance ('MonadHaskeline' m) => 'MonadHaskeline' (AppT env m)
-- @
--
-- This allows typical usage with some custom type that picks up the
-- instances automatically.

-- | @since 0.1
instance {-# OVERLAPPABLE #-} (MonadHaskeline m) => MonadHaskeline (ReaderT e m) where
  haveTerminalUI = lift haveTerminalUI
  {-# INLINEABLE haveTerminalUI #-}
  getInputLine = lift . getInputLine
  {-# INLINEABLE getInputLine #-}
  getInputLineWithInitial s = lift . getInputLineWithInitial s
  {-# INLINEABLE getInputLineWithInitial #-}
  getInputChar = lift . getInputChar
  {-# INLINEABLE getInputChar #-}
  getPassword c = lift . getPassword c
  {-# INLINEABLE getPassword #-}
  waitForAnyKey = lift . waitForAnyKey
  {-# INLINEABLE waitForAnyKey #-}
  outputStr = lift . outputStr
  {-# INLINEABLE outputStr #-}
  outputStrLn = lift . outputStrLn
  {-# INLINEABLE outputStrLn #-}
  getExternalPrint = lift getExternalPrint
  {-# INLINEABLE getExternalPrint #-}
  getHistory = lift getHistory
  {-# INLINEABLE getHistory #-}
  putHistory = lift . putHistory
  {-# INLINEABLE putHistory #-}
  modifyHistory = lift . modifyHistory
  {-# INLINEABLE modifyHistory #-}
  withInterrupt rdr = ask >>= \env -> lift $ withInterrupt $ runReaderT rdr env
  {-# INLINEABLE withInterrupt #-}
  handleInterrupt m1 m2 =
    ask >>= \env ->
      lift $ handleInterrupt (runReaderT m1 env) (runReaderT m2 env)
  {-# INLINEABLE handleInterrupt #-}

-- | @since 0.1
instance {-# OVERLAPPING #-} (MonadIO m, MonadMask m) => MonadHaskeline (ReaderT (InputTEnv m) m) where
  haveTerminalUI = liftReaderT H.haveTerminalUI
  {-# INLINEABLE haveTerminalUI #-}
  getInputLine = liftReaderT . H.getInputLine
  {-# INLINEABLE getInputLine #-}
  getInputLineWithInitial s = liftReaderT . H.getInputLineWithInitial s
  {-# INLINEABLE getInputLineWithInitial #-}
  getInputChar = liftReaderT . H.getInputChar
  {-# INLINEABLE getInputChar #-}
  getPassword c = liftReaderT . H.getPassword c
  {-# INLINEABLE getPassword #-}
  waitForAnyKey = liftReaderT . H.waitForAnyKey
  {-# INLINEABLE waitForAnyKey #-}
  outputStr = liftReaderT . H.outputStr
  {-# INLINEABLE outputStr #-}
  outputStrLn = liftReaderT . H.outputStrLn
  {-# INLINEABLE outputStrLn #-}
  getExternalPrint = liftReaderT H.getExternalPrint
  {-# INLINEABLE getExternalPrint #-}
  getHistory = liftReaderT H.getHistory
  {-# INLINEABLE getHistory #-}
  putHistory = liftReaderT . H.putHistory
  {-# INLINEABLE putHistory #-}
  modifyHistory = liftReaderT . H.modifyHistory
  {-# INLINEABLE modifyHistory #-}
  withInterrupt r = ask >>= liftReaderT . H.withInterrupt . lift . runReaderT r
  {-# INLINEABLE withInterrupt #-}
  handleInterrupt r1 r2 = do
    env <- ask
    liftReaderT $
      H.handleInterrupt
        (lift $ runReaderT r1 env)
        (lift $ runReaderT r2 env)
  {-# INLINEABLE handleInterrupt #-}

liftReaderT :: InputT m a -> ReaderT (InputTEnv m) m a
liftReaderT = HR.toReaderT
{-# INLINEABLE liftReaderT #-}

-- | 'runInputTWith' with default haskeline settings.
--
-- @since 0.1
runInputTEnv :: (MonadIO m, MonadMask m) => (InputTEnv m -> m a) -> m a
runInputTEnv = runInputTEnvWith (H.runInputT H.defaultSettings)
{-# INLINEABLE runInputTEnv #-}

-- | Runs 'Control.Monad.Reader.ReaderT' 'InputTEnv' in 'IO' with 'InputT'
-- runner.
--
-- @since 0.1
runInputTEnvWith ::
  -- | 'InputT' Runner.
  (InputT m a -> m a) ->
  -- | Action.
  (InputTEnv m -> m a) ->
  -- | IO Result.
  m a
runInputTEnvWith runInput onEnv = runInput $ HR.fromReaderT $ ReaderT onEnv
{-# INLINEABLE runInputTEnvWith #-}
