-- | Provides the 'MonadProcess' typeclass for process effects.
--
-- @since 0.1
module Effects.System.Process
  ( -- * Effect
    MonadProcess (..),

    -- * Types
    ProcessConfig,
    StreamSpec,
    StreamType,
    Process,

    -- * ProcessConfig
    P.proc,
    P.shell,

    -- * Setters
    P.setStdin,
    P.setStdout,
    P.setStderr,
    P.setWorkingDir,
    P.setWorkingDirInherit,
    P.setEnv,
    P.setEnvInherit,
    P.setCloseFds,
    P.setCreateGroup,
    P.setDelegateCtlc,
    P.setDetachConsole,
    P.setCreateNewConsole,
    P.setNewSession,
    P.setChildGroup,
    P.setChildGroupInherit,
    P.setChildUser,
    P.setChildUserInherit,

    -- * Stream specs

    -- ** Built-in stream specs
    P.inherit,
    P.nullStream,
    P.closed,
    P.byteStringInput,
    P.byteStringOutput,
    P.createPipe,
    P.useHandleOpen,
    P.useHandleClose,

    -- ** Create your own stream spec
    P.mkStreamSpec,
    P.mkPipeStreamSpec,

    -- * Launch a process
    runProcess,
    readProcess,
    readProcessStdout,
    readProcessStderr,
    withProcessWait,

    -- * Exception-throwing functions
    runProcess_,
    readProcess_,
    readProcessStdout_,
    readProcessStderr_,
    withProcessWait_,
    withProcessTerm_,

    -- * Interact with a process

    -- ** Process exit code
    waitExitCode,
    P.waitExitCodeSTM,
    getExitCode,
    P.getExitCodeSTM,
    checkExitCode,
    P.checkExitCodeSTM,

    -- ** Process streams
    P.getStdin,
    P.getStdout,
    P.getStderr,

    -- * Exceptions
    P.ExitCodeException (..),
    P.ByteStringOutputException (..),

    -- * Re-exports
    P.ExitCode (..),
    P.StdStream (..),

    -- * Unsafe functions
    P.unsafeProcessHandle,
  )
where

import Control.Exception.Safe (MonadMask, bracket, finally)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Lazy qualified as BSL
import Effects.Concurrent.STM (MonadSTM (..))
import Effects.Exception (addCS)
import GHC.Conc (catchSTM, throwSTM)
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import System.Process.Typed
  ( ExitCodeException (..),
    Process,
    ProcessConfig,
    StreamSpec,
    StreamType,
  )
import System.Process.Typed qualified as P

-- | Effect for launching processes. API largely follows
-- [typed-process](https://hackage.haskell.org/package/typed-process).
--
-- @since 0.1
class (Monad m) => MonadProcess m where
  -- | Same as 'readProcess', but interleaves stderr with stdout.
  --
  -- Motivation: Use this function if you need stdout interleaved with stderr
  -- output (e.g. from an HTTP server) in order to debug failures.
  --
  -- @since 0.1
  readProcessInterleaved ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m (ExitCode, BSL.ByteString)

  -- | Uses the bracket pattern to call 'startProcess' and ensures that
  -- 'stopProcess' is called.
  --
  -- This function is usually /not/ what you want. You're likely better
  -- off using 'withProcessWait'. See
  -- <https://github.com/fpco/typed-process/issues/25>.
  --
  -- @since 0.1
  withProcessTerm ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    m a

  -- | Launch a process based on the given 'ProcessConfig'. You should
  -- ensure that you call 'stopProcess' on the result. It's usually
  -- better to use one of the functions in this module which ensures
  -- 'stopProcess' is called, such as 'withProcessWait'.
  --
  -- @since 0.1
  startProcess ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    m (Process stdin stdout stderr)

  -- | Close a process and release any resources acquired. This will
  -- ensure 'P.terminateProcess' is called, wait for the process to
  -- actually exit, and then close out resources allocated for the
  -- streams. In the event of any cleanup exceptions being thrown this
  -- will throw an exception.
  --
  -- @since 0.1
  stopProcess :: (HasCallStack) => Process stdin stdout stderr -> m ()

  -- | Same as 'readProcessInterleaved', but instead of returning the 'ExitCode',
  -- checks it with 'checkExitCode'.
  --
  -- Exceptions thrown by this function will include stdout.
  --
  -- @since 0.1
  readProcessInterleaved_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m BSL.ByteString

-- | @since 0.1
instance MonadProcess IO where
  readProcessInterleaved = addCS . P.readProcessInterleaved
  {-# INLINEABLE readProcessInterleaved #-}
  withProcessTerm pc = addCS . P.withProcessTerm pc
  {-# INLINEABLE withProcessTerm #-}
  startProcess = addCS . P.startProcess
  {-# INLINEABLE startProcess #-}
  stopProcess = addCS . P.stopProcess
  {-# INLINEABLE stopProcess #-}
  readProcessInterleaved_ = addCS . P.readProcessInterleaved_
  {-# INLINEABLE readProcessInterleaved_ #-}

-- | @since 0.1
instance (MonadProcess m) => MonadProcess (ReaderT env m) where
  readProcessInterleaved = lift . readProcessInterleaved
  {-# INLINEABLE readProcessInterleaved #-}
  withProcessTerm pc onProcess =
    ask >>= \e -> lift (withProcessTerm pc ((`runReaderT` e) . onProcess))
  {-# INLINEABLE withProcessTerm #-}
  startProcess = lift . startProcess
  {-# INLINEABLE startProcess #-}
  stopProcess = lift . stopProcess
  {-# INLINEABLE stopProcess #-}
  readProcessInterleaved_ = lift . readProcessInterleaved_
  {-# INLINEABLE readProcessInterleaved_ #-}

-- | Run the given process, wait for it to exit, and returns its
-- 'ExitCode'.
--
-- @since 0.1
runProcess ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  m ExitCode
runProcess pc = withProcessTerm pc waitExitCode
{-# INLINEABLE runProcess #-}

-- | Run a process, capture its standard output and error as a
-- 'BSL.ByteString', wait for it to complete, and then return its exit
-- code, output, and error.
--
-- Note that any previously used 'setStdout' or 'setStderr' will be
-- overridden.
--
-- @since 0.1
readProcess ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  m (ExitCode, BSL.ByteString, BSL.ByteString)
readProcess pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,,)
        <$> P.waitExitCodeSTM p
        <*> P.getStdout p
        <*> P.getStderr p
  where
    pc' =
      P.setStdout P.byteStringOutput $
        P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcess #-}

-- | Same as 'readProcess', but only read the stdout of the process.
-- Original settings for stderr remain.
--
-- @since 0.1
readProcessStdout ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderr ->
  m (ExitCode, BSL.ByteString)
readProcessStdout pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,)
        <$> P.waitExitCodeSTM p
        <*> P.getStdout p
  where
    pc' = P.setStdout P.byteStringOutput pc
{-# INLINEABLE readProcessStdout #-}

-- | Same as 'readProcess', but only read the stderr of the process.
-- Original settings for stdout remain.
--
-- @since 0.1
readProcessStderr ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderrIgnored ->
  m (ExitCode, BSL.ByteString)
readProcessStderr pc =
  withProcessTerm pc' $ \p ->
    atomically $
      (,)
        <$> P.waitExitCodeSTM p
        <*> P.getStderr p
  where
    pc' = P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcessStderr #-}

-- | Uses the bracket pattern to call 'startProcess'. Unlike
-- 'withProcessTerm', this function will wait for the child process to
-- exit, and only kill it with 'stopProcess' in the event that the
-- inner function throws an exception.
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.1
withProcessWait ::
  (HasCallStack, MonadMask m, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessWait config f =
  bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* waitExitCode p)
{-# INLINEABLE withProcessWait #-}

-- | Same as 'runProcess', but instead of returning the 'ExitCode', checks it
-- with 'checkExitCode'.
--
-- @since 0.1
runProcess_ ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  m ()
runProcess_ pc = withProcessTerm pc checkExitCode
{-# INLINEABLE runProcess_ #-}

-- | Same as 'readProcess', but instead of returning the 'ExitCode',
-- checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stdout and stderr.
--
-- @since 0.1
readProcess_ ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  m (BSL.ByteString, BSL.ByteString)
readProcess_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stdout <- P.getStdout p
    stderr <- P.getStderr p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStdout = stdout,
            eceStderr = stderr
          }
    return (stdout, stderr)
  where
    pc' =
      P.setStdout P.byteStringOutput $
        P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcess_ #-}

-- | Same as 'readProcessStdout', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stdout.
--
-- @since 0.1
readProcessStdout_ ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdoutIgnored stderr ->
  m BSL.ByteString
readProcessStdout_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stdout <- P.getStdout p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStdout = stdout
          }
    return stdout
  where
    pc' = P.setStdout P.byteStringOutput pc
{-# INLINEABLE readProcessStdout_ #-}

-- | Same as 'readProcessStderr', but instead of returning the
-- 'ExitCode', checks it with 'checkExitCode'.
--
-- Exceptions thrown by this function will include stderr.
--
-- @since 0.1
readProcessStderr_ ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderrIgnored ->
  m BSL.ByteString
readProcessStderr_ pc =
  withProcessTerm pc' $ \p -> atomically $ do
    stderr <- P.getStderr p
    P.checkExitCodeSTM p `catchSTM` \ece ->
      throwSTM
        ece
          { eceStderr = stderr
          }
    return stderr
  where
    pc' = P.setStderr P.byteStringOutput pc
{-# INLINEABLE readProcessStderr_ #-}

-- | Same as 'withProcessWait', but also calls 'checkExitCode'
--
-- @since 0.1
withProcessWait_ ::
  (HasCallStack, MonadMask m, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessWait_ config f =
  bracket
    (startProcess config)
    stopProcess
    (\p -> f p <* checkExitCode p)
{-# INLINEABLE withProcessWait_ #-}

-- | Lifted Same as 'withProcessTerm', but also calls 'checkExitCode'
--
-- To interact with a @Process@ use the functions from the section
-- [Interact with a process](#interactwithaprocess).
--
-- @since 0.1
withProcessTerm_ ::
  (MonadMask m, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessTerm_ config =
  bracket
    (startProcess config)
    (\p -> stopProcess p `finally` checkExitCode p)
{-# INLINEABLE withProcessTerm_ #-}

-- | Wait for the process to exit and then return its 'ExitCode'.
--
-- @since 0.1
waitExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m ExitCode
waitExitCode = atomically . P.waitExitCodeSTM
{-# INLINEABLE waitExitCode #-}

-- | Check if a process has exited and, if so, return its 'ExitCode'.
--
-- @since 0.1
getExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m (Maybe ExitCode)
getExitCode = atomically . P.getExitCodeSTM
{-# INLINEABLE getExitCode #-}

-- | Wait for a process to exit, and ensure that it exited successfully.
-- If not, throws an 'ExitCodeException'.
--
-- Exceptions thrown by this function will not include stdout or stderr
-- (This prevents unbounded memory usage from reading them into memory).
-- However, some callers such as 'readProcess_' catch the exception, add the
-- stdout and stderr, and rethrow.
--
-- @since 0.1
checkExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m ()
checkExitCode = atomically . P.checkExitCodeSTM
{-# INLINEABLE checkExitCode #-}
