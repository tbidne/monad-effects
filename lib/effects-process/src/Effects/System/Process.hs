{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | Provides the 'MonadProcess' typeclass for process effects.
--
-- @since 0.1
module Effects.System.Process
  ( -- * Effect
    MonadProcess (..),

    -- * Running sub-processes
    createProcess,
    createProcess_,
    P.shell,
    P.proc,
    P.CreateProcess (..),
    P.CmdSpec (..),
    P.StdStream (..),
    P.ProcessHandle,

    -- * Simpler functions for common tasks
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
    withCreateProcess,
    cleanupProcess,

    -- * Related utilities
    P.showCommandForUser,
    P.Pid,
    getPid,
    getCurrentPid,
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,
    createPipe,
    createPipeFd,

    -- * Re-exports
    Handle,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Posix.Internals (FD)
import System.Process (CreateProcess, Pid, ProcessHandle)
import System.Process qualified as P

-- | Effect for launching processes. API largely follows
-- [process](https://hackage.haskell.org/package/process).
--
-- @since 0.1
class (Monad m) => MonadProcess m where
  -- | @since 0.1
  createProcess ::
    (HasCallStack) =>
    -- | .
    CreateProcess ->
    m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

  -- | @since 0.1
  createProcess_ ::
    (HasCallStack) =>
    -- | .
    String ->
    CreateProcess ->
    m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

  -- | @since 0.1
  callProcess :: (HasCallStack) => FilePath -> [String] -> m ()

  -- | @since 0.1
  callCommand :: (HasCallStack) => String -> m ()

  -- | @since 0.1
  spawnProcess :: (HasCallStack) => FilePath -> [String] -> m ProcessHandle

  -- | @since 0.1
  spawnCommand :: (HasCallStack) => String -> m ProcessHandle

  -- | @since 0.1
  readCreateProcess :: (HasCallStack) => CreateProcess -> String -> m String

  -- | @since 0.1
  readProcess ::
    (HasCallStack) =>
    -- | .
    FilePath ->
    [String] ->
    String ->
    m String

  -- | @since 0.1
  readCreateProcessWithExitCode ::
    (HasCallStack) =>
    -- | .
    CreateProcess ->
    String ->
    m (ExitCode, String, String)

  -- | @since 0.1
  readProcessWithExitCode ::
    (HasCallStack) =>
    -- | .
    FilePath ->
    [String] ->
    String ->
    m (ExitCode, String, String)

  -- | @since 0.1
  withCreateProcess ::
    (HasCallStack) =>
    -- | .
    CreateProcess ->
    (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a) ->
    m a

  -- | @since 0.1
  cleanupProcess ::
    (HasCallStack) =>
    -- | .
    (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ->
    m ()

  -- | @since 0.1
  getPid :: (HasCallStack) => ProcessHandle -> m (Maybe Pid)

  -- | @since 0.1
  getCurrentPid :: (HasCallStack) => m Pid

  -- | @since 0.1
  waitForProcess :: (HasCallStack) => ProcessHandle -> m ExitCode

  -- | @since 0.1
  getProcessExitCode :: (HasCallStack) => ProcessHandle -> m (Maybe ExitCode)

  -- | @since 0.1
  terminateProcess :: (HasCallStack) => ProcessHandle -> m ()

  -- | @since 0.1
  interruptProcessGroupOf :: (HasCallStack) => ProcessHandle -> m ()

  -- | @since 0.1
  createPipe :: (HasCallStack) => m (Handle, Handle)

  -- | @since 0.1
  createPipeFd :: (HasCallStack) => m (FD, FD)

-- | @since 0.1
instance MonadProcess IO where
  createProcess = P.createProcess
  {-# INLINEABLE createProcess #-}

  createProcess_ = P.createProcess_
  {-# INLINEABLE createProcess_ #-}

  callProcess = P.callProcess
  {-# INLINEABLE callProcess #-}

  callCommand = P.callCommand
  {-# INLINEABLE callCommand #-}

  spawnProcess = P.spawnProcess
  {-# INLINEABLE spawnProcess #-}

  spawnCommand = P.spawnCommand
  {-# INLINEABLE spawnCommand #-}

  readCreateProcess = P.readCreateProcess
  {-# INLINEABLE readCreateProcess #-}

  readProcess = P.readProcess
  {-# INLINEABLE readProcess #-}

  readCreateProcessWithExitCode = P.readCreateProcessWithExitCode
  {-# INLINEABLE readCreateProcessWithExitCode #-}

  readProcessWithExitCode = P.readProcessWithExitCode
  {-# INLINEABLE readProcessWithExitCode #-}

  withCreateProcess = P.withCreateProcess
  {-# INLINEABLE withCreateProcess #-}

  cleanupProcess = P.cleanupProcess
  {-# INLINEABLE cleanupProcess #-}

  getPid = P.getPid
  {-# INLINEABLE getPid #-}

  getCurrentPid = P.getCurrentPid
  {-# INLINEABLE getCurrentPid #-}

  waitForProcess = P.waitForProcess
  {-# INLINEABLE waitForProcess #-}

  getProcessExitCode = P.getProcessExitCode
  {-# INLINEABLE getProcessExitCode #-}

  terminateProcess = P.terminateProcess
  {-# INLINEABLE terminateProcess #-}

  interruptProcessGroupOf = P.interruptProcessGroupOf
  {-# INLINEABLE interruptProcessGroupOf #-}

  createPipe = P.createPipe
  {-# INLINEABLE createPipe #-}

  createPipeFd = P.createPipeFd
  {-# INLINEABLE createPipeFd #-}

-- | @since 0.1
instance (MonadProcess m) => MonadProcess (ReaderT e m) where
  createProcess = lift . createProcess
  {-# INLINEABLE createProcess #-}

  createProcess_ x1 = lift . createProcess_ x1
  {-# INLINEABLE createProcess_ #-}

  callProcess x1 = lift . callProcess x1
  {-# INLINEABLE callProcess #-}

  callCommand = lift . callCommand
  {-# INLINEABLE callCommand #-}

  spawnProcess x1 = lift . spawnProcess x1
  {-# INLINEABLE spawnProcess #-}

  spawnCommand = lift . spawnCommand
  {-# INLINEABLE spawnCommand #-}

  readCreateProcess x1 = lift . readCreateProcess x1
  {-# INLINEABLE readCreateProcess #-}

  readProcess x1 x2 = lift . readProcess x1 x2
  {-# INLINEABLE readProcess #-}

  readCreateProcessWithExitCode x1 = lift . readCreateProcessWithExitCode x1
  {-# INLINEABLE readCreateProcessWithExitCode #-}

  readProcessWithExitCode x1 x2 = lift . readProcessWithExitCode x1 x2
  {-# INLINEABLE readProcessWithExitCode #-}

  withCreateProcess cp f =
    ask >>= \env ->
      lift (withCreateProcess cp (\h1 h2 h3 ph -> runReaderT (f h1 h2 h3 ph) env))
  {-# INLINEABLE withCreateProcess #-}

  cleanupProcess = lift . cleanupProcess
  {-# INLINEABLE cleanupProcess #-}

  getPid = lift . getPid
  {-# INLINEABLE getPid #-}

  getCurrentPid = lift getCurrentPid
  {-# INLINEABLE getCurrentPid #-}

  waitForProcess = lift . waitForProcess
  {-# INLINEABLE waitForProcess #-}

  getProcessExitCode = lift . getProcessExitCode
  {-# INLINEABLE getProcessExitCode #-}

  terminateProcess = lift . terminateProcess
  {-# INLINEABLE terminateProcess #-}

  interruptProcessGroupOf = lift . interruptProcessGroupOf
  {-# INLINEABLE interruptProcessGroupOf #-}

  createPipe = lift createPipe
  {-# INLINEABLE createPipe #-}

  createPipeFd = lift createPipeFd
  {-# INLINEABLE createPipeFd #-}
