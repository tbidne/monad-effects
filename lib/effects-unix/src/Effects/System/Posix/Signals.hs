module Effects.System.Posix.Signals
  ( -- * Effect
    MonadPosixSignals (..),

    -- * Handler
    Handler (..),
    mapHandler,
    handlerToPosix,
    handlerFromPosix,

    -- * Re-exports
    Signal,
    SignalSet,
    ProcessID,
    ProcessGroupID,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import GHC.Stack.Types (HasCallStack)
import System.Posix.Signals (Signal, SignalInfo, SignalSet)
import System.Posix.Signals qualified as Signals
import System.Posix.Types (ProcessGroupID, ProcessID)

{- HLINT ignore "Redundant bracket" -}

-- | Class for unix signal effects.
--
-- @since 0.1
class (Monad m) => MonadPosixSignals m where
  -- | @since 0.1
  raiseSignal :: (HasCallStack) => Signal -> m ()

  -- | @since 0.1
  signalProcess :: Signal -> ProcessID -> m ()

  -- | @since 0.1
  signalProcessGroup :: Signal -> ProcessGroupID -> m ()

  -- | @since 0.1
  installHandler :: Signal -> Handler m -> Maybe SignalSet -> m (Handler m)

  -- | @since 0.1
  getSignalMask :: m SignalSet

  -- | @since 0.1
  setSignalMask :: SignalSet -> m ()

  -- | @since 0.1
  blockSignals :: SignalSet -> m ()

  -- | @since 0.1
  unblockSignals :: SignalSet -> m ()

  -- | @since 0.1
  scheduleAlarm :: Int -> m Int

  -- | @since 0.1
  getPendingSignals :: m SignalSet

  -- | @since 0.1
  awaitSignal :: Maybe SignalSet -> m ()

  -- | @since 0.1
  setStoppedChildFlag :: Bool -> m Bool

  -- | @since 0.1
  queryStoppedChildFlag :: m Bool

-- | @since 0.1
instance MonadPosixSignals IO where
  raiseSignal = Signals.raiseSignal
  {-# INLINEABLE raiseSignal #-}

  signalProcess = Signals.signalProcess
  {-# INLINEABLE signalProcess #-}

  signalProcessGroup = Signals.signalProcessGroup
  {-# INLINEABLE signalProcessGroup #-}

  installHandler s h =
    fmap handlerFromPosix . Signals.installHandler s (handlerToPosix h)
  {-# INLINEABLE installHandler #-}

  getSignalMask = Signals.getSignalMask
  {-# INLINEABLE getSignalMask #-}

  setSignalMask = Signals.setSignalMask
  {-# INLINEABLE setSignalMask #-}

  blockSignals = Signals.blockSignals
  {-# INLINEABLE blockSignals #-}

  unblockSignals = Signals.unblockSignals
  {-# INLINEABLE unblockSignals #-}

  scheduleAlarm = Signals.scheduleAlarm
  {-# INLINEABLE scheduleAlarm #-}

  getPendingSignals = Signals.getPendingSignals
  {-# INLINEABLE getPendingSignals #-}

  awaitSignal = Signals.awaitSignal
  {-# INLINEABLE awaitSignal #-}

  setStoppedChildFlag = Signals.setStoppedChildFlag
  {-# INLINEABLE setStoppedChildFlag #-}

  queryStoppedChildFlag = Signals.queryStoppedChildFlag
  {-# INLINEABLE queryStoppedChildFlag #-}

-- | @since 0.1
instance (MonadPosixSignals m) => MonadPosixSignals (ReaderT e m) where
  raiseSignal = lift . raiseSignal
  {-# INLINEABLE raiseSignal #-}

  signalProcess s = lift . signalProcess s
  {-# INLINEABLE signalProcess #-}

  signalProcessGroup s = lift . signalProcessGroup s
  {-# INLINEABLE signalProcessGroup #-}

  installHandler s h m =
    ask >>= \env ->
      lift $ hFromM <$> (installHandler s (hToM env h) m)
    where
      hFromM :: Handler m -> Handler (ReaderT e m)
      hFromM = mapHandler lift

      hToM :: e -> Handler (ReaderT e m) -> Handler m
      hToM env = mapHandler (`runReaderT` env)
  {-# INLINEABLE installHandler #-}

  getSignalMask = lift getSignalMask
  {-# INLINEABLE getSignalMask #-}

  setSignalMask = lift . setSignalMask
  {-# INLINEABLE setSignalMask #-}

  blockSignals = lift . blockSignals
  {-# INLINEABLE blockSignals #-}

  unblockSignals = lift . unblockSignals
  {-# INLINEABLE unblockSignals #-}

  scheduleAlarm = lift . scheduleAlarm
  {-# INLINEABLE scheduleAlarm #-}

  getPendingSignals = lift getPendingSignals
  {-# INLINEABLE getPendingSignals #-}

  awaitSignal = lift . awaitSignal
  {-# INLINEABLE awaitSignal #-}

  setStoppedChildFlag = lift . setStoppedChildFlag
  {-# INLINEABLE setStoppedChildFlag #-}

  queryStoppedChildFlag = lift queryStoppedChildFlag
  {-# INLINEABLE queryStoppedChildFlag #-}

-- | @since 0.1
data Handler m
  = Default
  | Ignore
  | Catch (m ())
  | CatchOnce (m ())
  | CatchInfo (SignalInfo -> m ())
  | CatchInfoOnce (SignalInfo -> m ())

mapHandler :: (forall x. m x -> n x) -> Handler m -> Handler n
mapHandler f = \case
  Default -> Default
  Ignore -> Ignore
  Catch x -> Catch $ f x
  CatchOnce x -> CatchOnce $ f x
  CatchInfo x -> CatchInfo $ f . x
  CatchInfoOnce x -> CatchInfoOnce $ f . x

handlerToPosix :: Handler IO -> Signals.Handler
handlerToPosix = \case
  Default -> Signals.Default
  Ignore -> Signals.Ignore
  Catch x -> Signals.Catch x
  CatchOnce x -> Signals.CatchOnce x
  CatchInfo x -> Signals.CatchInfo x
  CatchInfoOnce x -> Signals.CatchInfoOnce x

handlerFromPosix :: Signals.Handler -> Handler IO
handlerFromPosix = \case
  Signals.Default -> Default
  Signals.Ignore -> Ignore
  Signals.Catch x -> Catch x
  Signals.CatchOnce x -> CatchOnce x
  Signals.CatchInfo x -> CatchInfo x
  Signals.CatchInfoOnce x -> CatchInfoOnce x
