{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

{- ORMOLU_DISABLE -}

-- | Provides the 'MonadTypedProcess' typeclass for process effects.
--
-- @since 0.1
module Effects.Process.Typed
  ( -- * Effect
    MonadTypedProcess (..),

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
#if !WINDOWS
    P.setChildGroup,
    P.setChildGroupInherit,
    P.setChildUser,
    P.setChildUserInherit,
#endif

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
    readProcessInterleaved,
    withProcessWait,
    withProcessTerm,
    startProcess,
    stopProcess,

    -- * Exception-throwing functions
    runProcess_,
    readProcess_,
    readProcessStdout_,
    readProcessStderr_,
    readProcessInterleaved_,
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

{- ORMOLU_ENABLE -}

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.ByteString.Lazy qualified as BSL
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode)
import System.Process.Typed
  ( Process,
    ProcessConfig,
    StreamSpec,
    StreamType,
  )
import System.Process.Typed qualified as P

-- | Effect for launching processes. API largely follows
-- [typed-process](https://hackage.haskell.org/package/typed-process).
--
-- @since 0.1
class (Monad m) => MonadTypedProcess m where
  -- | Lifted 'P.runProcess'.
  --
  -- @since 0.1
  runProcess ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    m ExitCode

  -- | Lifted 'P.readProcess'.
  --
  -- @since 0.1
  readProcess ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m (ExitCode, BSL.ByteString, BSL.ByteString)

  -- | Lifted 'P.readProcessStdout'.
  --
  -- @since 0.1
  readProcessStdout ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderr ->
    m (ExitCode, BSL.ByteString)

  -- | Lifted 'P.readProcessStderr'.
  --
  -- @since 0.1
  readProcessStderr ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderrIgnored ->
    m (ExitCode, BSL.ByteString)

  -- | Lifted 'P.readProcessInterleaved'.
  --
  -- @since 0.1
  readProcessInterleaved ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m (ExitCode, BSL.ByteString)

  -- | Lifted 'P.withProcessWait'.
  --
  -- @since 0.1
  withProcessWait ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    m a

  -- | Lifted 'P.withProcessTerm'.
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

  -- | Lifted 'P.stopProcess'.
  --
  -- @since 0.1
  stopProcess ::
    (HasCallStack) =>
    -- | .
    Process stdin stdout stderr ->
    m ()

  -- | Lifted 'P.runProcess_'.
  --
  -- @since 0.1
  runProcess_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    m ()

  -- | Lifted 'P.readProcess_'.
  --
  -- @since 0.1
  readProcess_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m (BSL.ByteString, BSL.ByteString)

  -- | Lifted 'P.readProcessStdout_'.
  --
  -- @since 0.1
  readProcessStdout_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderr ->
    m BSL.ByteString

  -- | Lifted 'P.readProcessStderr_'.
  --
  -- @since 0.1
  readProcessStderr_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderrIgnored ->
    m BSL.ByteString

  -- | Lifted 'P.readProcessInterleaved_'.
  --
  -- @since 0.1
  readProcessInterleaved_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdoutIgnored stderrIgnored ->
    m BSL.ByteString

  -- | Lifted 'P.withProcessWait_'.
  --
  -- @since 0.1
  withProcessWait_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    m a

  -- | Lifted 'P.withProcessTerm_'.
  --
  -- @since 0.1
  withProcessTerm_ ::
    (HasCallStack) =>
    -- | .
    ProcessConfig stdin stdout stderr ->
    (Process stdin stdout stderr -> m a) ->
    m a

  -- | Lifted 'P.waitExitCode'.
  --
  -- @since 0.1
  waitExitCode ::
    (HasCallStack) =>
    -- | .
    Process stdin stdout stderr ->
    m ExitCode

  -- | Lifted 'P.getExitCode'.
  --
  -- @since 0.1
  getExitCode ::
    (HasCallStack) =>
    -- | .
    Process stdin stdout stderr ->
    m (Maybe ExitCode)

  -- | Lifted 'P.checkExitCode'.
  --
  -- @since 0.1
  checkExitCode ::
    (HasCallStack) =>
    -- | .
    Process stdin stdout stderr ->
    m ()

-- | @since 0.1
instance MonadTypedProcess IO where
  runProcess = P.runProcess
  {-# INLINEABLE runProcess #-}
  readProcess = P.readProcess
  {-# INLINEABLE readProcess #-}
  readProcessStdout = P.readProcessStdout
  {-# INLINEABLE readProcessStdout #-}
  readProcessStderr = P.readProcessStderr
  {-# INLINEABLE readProcessStderr #-}
  readProcessInterleaved = P.readProcessInterleaved
  {-# INLINEABLE readProcessInterleaved #-}
  withProcessWait = P.withProcessWait
  {-# INLINEABLE withProcessWait #-}
  withProcessTerm = P.withProcessTerm
  {-# INLINEABLE withProcessTerm #-}
  startProcess = P.startProcess
  {-# INLINEABLE startProcess #-}
  stopProcess = P.stopProcess
  {-# INLINEABLE stopProcess #-}
  runProcess_ = P.runProcess_
  {-# INLINEABLE runProcess_ #-}
  readProcess_ = P.readProcess_
  {-# INLINEABLE readProcess_ #-}
  readProcessStdout_ = P.readProcessStdout_
  {-# INLINEABLE readProcessStdout_ #-}
  readProcessStderr_ = P.readProcessStderr_
  {-# INLINEABLE readProcessStderr_ #-}
  readProcessInterleaved_ = P.readProcessInterleaved_
  {-# INLINEABLE readProcessInterleaved_ #-}
  withProcessWait_ = P.withProcessWait_
  {-# INLINEABLE withProcessWait_ #-}
  withProcessTerm_ = P.withProcessTerm_
  {-# INLINEABLE withProcessTerm_ #-}
  waitExitCode = P.waitExitCode
  {-# INLINEABLE waitExitCode #-}
  getExitCode = P.getExitCode
  {-# INLINEABLE getExitCode #-}
  checkExitCode = P.checkExitCode
  {-# INLINEABLE checkExitCode #-}

-- | @since 0.1
instance (MonadTypedProcess m) => MonadTypedProcess (ReaderT env m) where
  runProcess = lift . runProcess
  {-# INLINEABLE runProcess #-}
  readProcess = lift . readProcess
  {-# INLINEABLE readProcess #-}
  readProcessStdout = lift . readProcessStdout
  {-# INLINEABLE readProcessStdout #-}
  readProcessStderr = lift . readProcessStderr
  {-# INLINEABLE readProcessStderr #-}
  readProcessInterleaved = lift . readProcessInterleaved
  {-# INLINEABLE readProcessInterleaved #-}
  withProcessWait pc f =
    ask >>= \e -> lift $ withProcessWait pc (usingReaderT e . f)
  {-# INLINEABLE withProcessWait #-}
  withProcessTerm pc f =
    ask >>= \e -> lift $ withProcessTerm pc (usingReaderT e . f)
  {-# INLINEABLE withProcessTerm #-}
  startProcess = lift . startProcess
  {-# INLINEABLE startProcess #-}
  stopProcess = lift . stopProcess
  {-# INLINEABLE stopProcess #-}
  runProcess_ = lift . runProcess_
  {-# INLINEABLE runProcess_ #-}
  readProcess_ = lift . readProcess_
  {-# INLINEABLE readProcess_ #-}
  readProcessStdout_ = lift . readProcessStdout_
  {-# INLINEABLE readProcessStdout_ #-}
  readProcessStderr_ = lift . readProcessStderr_
  {-# INLINEABLE readProcessStderr_ #-}
  readProcessInterleaved_ = lift . readProcessInterleaved_
  {-# INLINEABLE readProcessInterleaved_ #-}
  withProcessWait_ pc f =
    ask >>= \e -> lift $ withProcessWait_ pc (usingReaderT e . f)
  {-# INLINEABLE withProcessWait_ #-}
  withProcessTerm_ pc f =
    ask >>= \e -> lift $ withProcessTerm_ pc (usingReaderT e . f)
  {-# INLINEABLE withProcessTerm_ #-}
  waitExitCode = lift . waitExitCode
  {-# INLINEABLE waitExitCode #-}
  getExitCode = lift . getExitCode
  {-# INLINEABLE getExitCode #-}
  checkExitCode = lift . checkExitCode
  {-# INLINEABLE checkExitCode #-}

usingReaderT :: e -> ReaderT e m a -> m a
usingReaderT = flip runReaderT
{-# INLINEABLE usingReaderT #-}
