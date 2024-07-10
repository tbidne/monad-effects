{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

module Effects.Haskeline

#if MIN_VERSION_base(4,19,0)

{-# WARNING in "x-experimental" "Effects.Haskeline is experimental and subject to change." #-}

#else

{-# WARNING "Effects.Haskeline is experimental (not deprecated) and subject to change." #-}

#endif

  ( -- * Class
    MonadHaskeline (..),

    -- * IO Runners
    H.runInputT,
    H.runInputTBehavior,
    H.runInputTBehaviorWithPrefs,

    -- * Config
    H.defaultSettings,
    H.defaultBehavior,
    H.defaultPrefs,
  )
where

{- ORMOLU_ENABLE -}

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import GHC.Stack (HasCallStack)
import System.Console.Haskeline (InputT)
import System.Console.Haskeline qualified as H
import System.Console.Haskeline.History (History)

-- NOTE: We implement most of the effectful functions from
-- System.Console.Haskeline. What is missing is functions of the form
-- 'foo :: Input m a -> m a' i.e. they run the input to produce a result in the
-- underlying monad.
--
-- It is not obvious how to add such functionality here since we chose to
-- return functions in the class parameter m, rather than InputT m. We
-- chose this because e.g. lifting to ReaderT was straightforward. Otherwise
-- we would have to write a 'hoist :: InputT (ReaderT e m) a -> InputT m a',
-- and this is also not obvious.
--
-- One way we might implement functions like foo is to include an associated
-- type 'type Base m' ana have 'foo :: m a -> Base m a'.
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

instance (MonadHaskeline m) => MonadHaskeline (ReaderT e m) where
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
