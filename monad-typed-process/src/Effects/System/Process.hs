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
  -- | Lifted 'P.readProcessInterleaved'.
  --
  -- @since 0.1
  readProcessInterleaved ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m (ExitCode, BSL.ByteString)

  -- | Lifted 'P.runProcess'.
  --
  -- @since 0.1
  withProcessTerm ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    m a

  -- | Lifted 'P.startProcess'.
  --
  -- @since 0.1
  startProcess ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    m (Process stdin stdout stderr)

  -- | Lifted 'P.startProcess'.
  --
  -- @since 0.1
  stopProcess :: (HasCallStack) => Process stdin stdout stderr -> m ()

  -- | Lifted 'P.readProcessInterleaved_'.
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

-- | Lifted 'P.runProcess'.
--
-- @since 0.1
runProcess ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  m ExitCode
runProcess pc = withProcessTerm pc waitExitCode
{-# INLINEABLE runProcess #-}

-- | Lifted 'P.readProcess'.
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

-- | Lifted 'P.readProcessStdout'.
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

-- | Lifted 'P.readProcessStderr'.
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

-- | Lifted 'P.withProcessWait'.
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

-- | Lifted 'P.runProcess'.
--
-- @since 0.1
runProcess_ ::
  (HasCallStack, MonadProcess m, MonadSTM m) =>
  -- | .
  ProcessConfig stdin stdout stderr ->
  m ()
runProcess_ pc = withProcessTerm pc checkExitCode
{-# INLINEABLE runProcess_ #-}

-- | Lifted 'P.readProcess_'.
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

-- | Lifted 'P.readProcessStdout_'.
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

-- | Lifted 'P.readProcessStderr_'.
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

-- | Lifted 'P.withProcessWait_'.
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

-- | Lifted 'P.withProcessTerm_'.
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

-- | Lifted 'P.waitExitCode'.
--
-- @since 0.1
waitExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m ExitCode
waitExitCode = atomically . P.waitExitCodeSTM
{-# INLINEABLE waitExitCode #-}

-- | Lifted 'getExitCode'.
--
-- @since 0.1
getExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m (Maybe ExitCode)
getExitCode = atomically . P.getExitCodeSTM
{-# INLINEABLE getExitCode #-}

-- | Lifted 'P.checkExitCode'.
--
-- @since 0.1
checkExitCode ::
  (HasCallStack, MonadSTM m) =>
  Process stdin stdout stderr ->
  m ()
checkExitCode = atomically . P.checkExitCodeSTM
{-# INLINEABLE checkExitCode #-}
