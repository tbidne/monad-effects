-- | Provides the 'MonadThread' typeclass.
--
-- @since 0.1
module Effects.Concurrent.Thread
  ( -- * Thread Effect
    MonadThread (..),
    microsleep,
    sleep,

    -- * Reexports
    Natural,
    ThreadId,

    -- * QSem Effect
    MonadQSem (..),

    -- * Reexports
    QSem,
    QSemN,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent qualified as CC
import Control.Concurrent.QSem (QSem)
import Control.Concurrent.QSem qualified as QSem
import Control.Concurrent.QSemN (QSemN)
import Control.Concurrent.QSemN qualified as QSemN
import Control.Exception (Exception)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (for_)
import Effects.Exception (addCS)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

-- | Represents thread effects.
--
-- @since 0.1
class (Monad m) => MonadThread m where
  -- | Suspends the current thread for a given number of microseconds
  -- (GHC only).
  --
  -- There is no guarantee that the thread will be rescheduled promptly
  -- when the delay has expired, but the thread will never continue to
  -- run /earlier/ than specified.
  --
  -- @since 0.1
  threadDelay :: (HasCallStack) => Int -> m ()

  -- | 'throwTo' raises an arbitrary exception in the target thread (GHC only).
  --
  -- Exception delivery synchronizes between the source and target thread:
  -- 'throwTo' does not return until the exception has been raised in the
  -- target thread. The calling thread can thus be certain that the target
  -- thread has received the exception. Exception delivery is also atomic
  -- with respect to other exceptions. Atomicity is a useful property to have
  -- when dealing with race conditions: e.g. if there are two threads that
  -- can kill each other, it is guaranteed that only one of the threads
  -- will get to kill the other.
  --
  -- Whatever work the target thread was doing when the exception was
  -- raised is not lost: the computation is suspended until required by
  -- another thread.
  --
  -- If the target thread is currently making a foreign call, then the
  -- exception will not be raised (and hence 'throwTo' will not return)
  -- until the call has completed. This is the case regardless of whether
  -- the call is inside a 'mask' or not. However, in GHC a foreign call
  -- can be annotated as @interruptible@, in which case a 'throwTo' will
  -- cause the RTS to attempt to cause the call to return; see the GHC
  -- documentation for more details.
  --
  -- Important note: the behaviour of 'throwTo' differs from that described in
  -- the paper \"Asynchronous exceptions in Haskell\"
  -- (<http://research.microsoft.com/~simonpj/Papers/asynch-exns.htm>).
  -- In the paper, 'throwTo' is non-blocking; but the library implementation
  -- adopts a more synchronous design in which 'throwTo' does not return until
  -- the exception is received by the target thread. The trade-off is
  -- discussed in Section 9 of the paper. Like any blocking operation,
  -- 'throwTo' is therefore interruptible (see Section 5.3 of the paper).
  -- Unlike other interruptible operations, however, 'throwTo' is /always/
  -- interruptible, even if it does not actually block.
  --
  -- There is no guarantee that the exception will be delivered promptly,
  -- although the runtime will endeavour to ensure that arbitrary
  -- delays don't occur. In GHC, an exception can only be raised when a
  -- thread reaches a /safe point/, where a safe point is where memory
  -- allocation occurs. Some loops do not perform any memory allocation
  -- inside the loop and therefore cannot be interrupted by a 'throwTo'.
  --
  -- If the target of 'throwTo' is the calling thread, then the behaviour
  -- is the same as 'Control.Exception.throwIO', except that the exception
  -- is thrown as an asynchronous exception. This means that if there is
  -- an enclosing pure computation, which would be the case if the current
  -- IO operation is inside 'unsafePerformIO' or 'unsafeInterleaveIO', that
  -- computation is not permanently replaced by the exception, but is
  -- suspended as if it had received an asynchronous exception.
  --
  -- Note that if 'throwTo' is called with the current thread as the
  -- target, the exception will be thrown even if the thread is currently
  -- inside 'mask' or 'uninterruptibleMask'.
  --
  -- @since 0.1
  throwTo :: (Exception e, HasCallStack) => ThreadId -> e -> m ()

  -- | Returns the number of Haskell threads that can run truly
  -- simultaneously (on separate physical processors) at any given time.
  -- To change this value, use 'setNumCapabilities'.
  --
  -- @since 0.1
  getNumCapabilities :: (HasCallStack) => m Int

  -- | Set the number of Haskell threads that can run truly simultaneously
  -- (on separate physical processors) at any given time. The number
  -- passed to `forkOn` is interpreted modulo this value. The initial
  -- value is given by the @+RTS -N@ runtime flag.
  --
  -- This is also the number of threads that will participate in parallel
  -- garbage collection. It is strongly recommended that the number of
  -- capabilities is not set larger than the number of physical processor
  -- cores, and it may often be beneficial to leave one or more cores free
  -- to avoid contention with other processes in the machine.
  --
  -- @since 0.1
  setNumCapabilities :: (HasCallStack) => Int -> m ()

  -- | Returns the number of the capability on which the thread is currently
  -- running, and a boolean indicating whether the thread is locked to
  -- that capability or not. A thread is locked to a capability if it
  -- was created with @forkOn@.
  --
  -- @since 0.1
  threadCapability :: (HasCallStack) => ThreadId -> m (Int, Bool)

-- | @since 0.1
instance MonadThread IO where
  threadDelay = addCS . CC.threadDelay
  {-# INLINEABLE threadDelay #-}
  throwTo tid = addCS . CC.throwTo tid
  {-# INLINEABLE throwTo #-}
  getNumCapabilities = addCS CC.getNumCapabilities
  {-# INLINEABLE getNumCapabilities #-}
  setNumCapabilities = addCS . CC.setNumCapabilities
  {-# INLINEABLE setNumCapabilities #-}
  threadCapability = addCS . CC.threadCapability
  {-# INLINEABLE threadCapability #-}

-- | @since 0.1
instance (MonadThread m) => MonadThread (ReaderT e m) where
  threadDelay = lift . threadDelay
  {-# INLINEABLE threadDelay #-}
  throwTo tid = lift . throwTo tid
  {-# INLINEABLE throwTo #-}
  getNumCapabilities = lift getNumCapabilities
  {-# INLINEABLE getNumCapabilities #-}
  setNumCapabilities = lift . setNumCapabilities
  {-# INLINEABLE setNumCapabilities #-}
  threadCapability = lift . threadCapability
  {-# INLINEABLE threadCapability #-}

-- | 'threadDelay' in terms of unbounded 'Natural' rather than 'Int' i.e.
-- runs sleep in the current thread for the specified number of microseconds.
--
-- @since 0.1
microsleep :: (HasCallStack, MonadThread m) => Natural -> m ()
microsleep n = for_ (natToInts n) threadDelay
{-# INLINEABLE microsleep #-}

-- | Runs sleep in the current thread for the specified number of
-- seconds.
--
-- @since 0.1
sleep :: (HasCallStack, MonadThread m) => Natural -> m ()
sleep = microsleep . (* 1_000_000)
{-# INLINEABLE sleep #-}

natToInts :: Natural -> [Int]
natToInts n
  | n > maxIntAsNat = maxInt : natToInts (n - maxIntAsNat)
  | otherwise = [n2i n]
  where
    maxInt :: Int
    maxInt = maxBound
    maxIntAsNat :: Natural
    maxIntAsNat = i2n maxInt

n2i :: Natural -> Int
n2i = fromIntegral

i2n :: Int -> Natural
i2n = fromIntegral

-- | Effect for simple semaphores.
--
-- @since 0.1
class (Monad m) => MonadQSem m where
  -- | Build a new 'QSem' with a supplied initial quantity.
  -- The initial quantity must be at least 0.
  --
  -- @since 0.1
  newQSem :: Int -> m QSem

  -- | Wait for a unit to become available.
  --
  -- @since 0.1
  waitQSem :: QSem -> m ()

  -- | Signal that a unit of the 'QSem' is available.
  --
  -- @since 0.1
  signalQSem :: QSem -> m ()

  -- | Build a new 'QSemN' with a supplied initial quantity.
  -- The initial quantity must be at least 0.
  --
  -- @since 0.1
  newQSemN :: Int -> m QSemN

  -- | Wait for the specified quantity to become available.
  --
  -- @since 0.1
  waitQSemN :: QSemN -> Int -> m ()

  -- | Signal that a given quantity is now available from the 'QSemN'.
  --
  -- @since 0.1
  signalQSemN :: QSemN -> Int -> m ()

-- | @since 0.1
instance MonadQSem IO where
  newQSem = QSem.newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = QSem.waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = QSem.signalQSem
  {-# INLINEABLE signalQSem #-}
  newQSemN = QSemN.newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN = QSemN.waitQSemN
  {-# INLINEABLE waitQSemN #-}
  signalQSemN = QSemN.signalQSemN
  {-# INLINEABLE signalQSemN #-}

-- | @since 0.1
instance (MonadQSem m) => MonadQSem (ReaderT e m) where
  newQSem = lift . newQSem
  {-# INLINEABLE newQSem #-}
  waitQSem = lift . waitQSem
  {-# INLINEABLE waitQSem #-}
  signalQSem = lift . signalQSem
  {-# INLINEABLE signalQSem #-}
  newQSemN = lift . newQSemN
  {-# INLINEABLE newQSemN #-}
  waitQSemN q = lift . waitQSemN q
  {-# INLINEABLE waitQSemN #-}
  signalQSemN q = lift . signalQSemN q
  {-# INLINEABLE signalQSemN #-}
