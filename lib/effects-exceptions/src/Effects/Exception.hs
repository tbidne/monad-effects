{-# LANGUAGE CPP #-}

-- | Exception handling. The interface here combines ideas from the following
-- two libraries in a somewhat idiosyncratic way:
--
-- * [exceptions](https://hackage.haskell.org/package/exceptions)
-- * [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
--
-- We have the following goals:
--
-- 1. Typeclass abstraction: @exceptions@'s
--    'MonadThrow'\/'MonadCatch'\/'MonadMask' hierarchy.
--
-- 2. Throw/catch exceptions in accordance with @safe-exceptions@: That is,
--    do not throw or catch async exceptions. We do not actually re-export
--    @safe-exceptions@ functions; the functions here are bespoke, primarily
--    to add `HasCallStack` constraints.
--
-- 3. Masking/bracket uses 'Ex.mask', not 'Ex.uninterruptibleMask': We take
--    the position that 'Ex.uninterruptibleMask' should /not/ be the default
--    behavior, as legitimate uses are rare, and for those corner cases,
--    deadlocks are worse than interruptible operations receiving async
--    exceptions.
--
-- The API exported here is not comprehensive vis-à-vis any of the mentioned
-- libraries, so if more functionality is required, a direct dependency
-- will be necessary.
--
-- Note that, like @safe-exceptions@, the typeclass methods for
-- 'MonadThrow' ('throwM') and 'MonadCatch' ('catch') are __not__ exported
-- here because they are overridden to prevent catching async exceptions.
-- This means that if one needs to manually write an instance for any of
-- those classes, then a dependency on @exceptions@ is required.
--
-- @since 0.1
module Effects.Exception
  ( -- * Global mechanisms
    MonadGlobalException (..),

    -- * Basic exceptions
    -- $basics

    -- ** Throwing
    MonadThrow,
    SafeEx.throwM,
    SafeEx.throwString,

    -- ** Catching
    MonadCatch,
    SafeEx.catch,
    SafeEx.catchAny,
    SafeEx.handle,
    SafeEx.handleAny,
    SafeEx.try,
    SafeEx.tryAny,
    Handler (..),
    SafeEx.catches,
    onException,
    SafeEx.catchIOError,

    -- ** Masking
    -- $masking
    MonadMask (..),
    Ex.ExitCase (..),
    Ex.mask_,
    Ex.uninterruptibleMask_,
    Ex.bracket,
    Ex.bracket_,
    Ex.finally,
    Ex.bracketOnError,

    -- ** Utils
    SafeEx.SyncExceptionWrapper,
    SafeEx.AsyncExceptionWrapper,
    SafeEx.toSyncException,
    SafeEx.toAsyncException,
    SafeEx.isSyncException,
    SafeEx.isAsyncException,

    -- ** Exiting
    -- $exit
    ExitCode (..),

    -- ** Functions
    exitFailure,
    exitSuccess,
    exitWith,

    -- * Misc
    CallStack,
    Exception (..),
    HasCallStack,
    SomeException,
    IOException,
    SafeEx.StringException,
  )
where

import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    Handler (Handler),
    MonadCatch,
    MonadMask (generalBracket, mask, uninterruptibleMask),
    MonadThrow,
    SomeException,
  )
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import GHC.Conc.Sync qualified as Sync
import GHC.IO.Exception (IOErrorType (InvalidArgument), IOException (IOError))
import GHC.Stack
  ( CallStack,
    HasCallStack,
    withFrozenCallStack,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-------------------------------------------------------------------------------
--                           MonadGlobalException                            --
-------------------------------------------------------------------------------

-- | Effect for global exception mechanisms.
--
-- @since 0.1
class (Monad m) => MonadGlobalException m where
  -- | Lifted 'Sync.setUncaughtExceptionHandler'.
  --
  -- @since 0.1
  setUncaughtExceptionHandler :: (SomeException -> m ()) -> m ()

  -- | Lifted 'Sync.getUncaughtExceptionHandler'.
  --
  -- @since 0.1
  getUncaughtExceptionHandler :: m (SomeException -> m ())

-- | @since 0.1
instance MonadGlobalException IO where
  setUncaughtExceptionHandler = Sync.setUncaughtExceptionHandler
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler = Sync.getUncaughtExceptionHandler
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | @since 0.1
instance (MonadGlobalException m) => MonadGlobalException (ReaderT env m) where
  setUncaughtExceptionHandler f =
    ask >>= \e ->
      lift $ setUncaughtExceptionHandler (\ex -> runReaderT (f ex) e)
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler =
    ask >>= \e -> lift (runReaderT getUncaughtExceptionHandler e)
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-------------------------------------------------------------------------------
--                                 Exceptions                                --
-------------------------------------------------------------------------------

-- $basics
-- The functionality here is a mix of @exceptions@ and @safe-exceptions@.
-- On the one hand, we reuse @exceptions@' typeclass hierarchy. On the other
-- hand, the functions we export (e.g. `throwM`, `catch`) are reimplemented
-- with @safe-exceptions@' philosophy: by default, do not throw or catch
-- async exceptions. The only reason we do not directly export all
-- @safe-exceptions@' functions is due to them missing `HasCallStack`
-- constraints.

-- | Run an action only if an exception is thrown in the main action. The
-- exception is not caught, simply rethrown.
--
-- /NOTE/ The action is only run if an /exception/ is thrown. If the monad
-- supports other ways of aborting the computation, the action won't run if
-- those other kinds of errors are thrown. See 'onError'.
--
-- /NOTE/ This is __not__ the @onException@ re-exported from either
-- @exceptions@ or @safe-exceptions@. Why? The @exceptions@ variant reuses its
-- @catch@, which does not have the async exception behavior we want
-- (i.e. do not catch async exceptions). The @safe-exceptions@ version, on the
-- other hand, use its bracket functions, meaning it performs
-- @uninterruptibleMask@, which we also do not want.
--
-- Hence this version, which is based on 'catchAny' i.e. it does not catch
-- any asynchronous exceptions, nor does it invoke any masking.
--
-- @since 0.1
onException :: forall m a b. (HasCallStack, MonadCatch m) => m a -> m b -> m a
onException action handler =
  withFrozenCallStack SafeEx.catchAny action (\e -> handler *> SafeEx.throwM e)
{-# INLINEABLE onException #-}

-------------------------------------------------------------------------------
--                                    Exit                                   --
-------------------------------------------------------------------------------

-- $exit
-- These functions represent 'System.Exit'. 'System.Exit.die' can be found
-- in "Effects.FileSystem.HandleWriter".

-- | The computation 'exitFailure' is equivalent to
-- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
-- where /exitfail/ is implementation-dependent.
--
-- @since 0.1
exitFailure :: (MonadThrow m) => m a
exitFailure = exitWith (ExitFailure 1)
{-# INLINEABLE exitFailure #-}

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- successfully.
--
-- @since 0.1
exitSuccess :: (MonadThrow m) => m a
exitSuccess = exitWith ExitSuccess
{-# INLINEABLE exitSuccess #-}

-- | Computation 'exitWith' @code@ throws 'ExitCode' @code@.
-- Normally this terminates the program, returning @code@ to the
-- program's caller.
--
-- On program termination, the standard 'Handle's 'stdout' and
-- 'stderr' are flushed automatically; any other buffered 'Handle's
-- need to be flushed manually, otherwise the buffered data will be
-- discarded.
--
-- A program that fails in any other way is treated as if it had
-- called 'exitFailure'.
-- A program that terminates successfully without calling 'exitWith'
-- explicitly is treated as if it had called 'exitWith' 'ExitSuccess'.
--
-- As an 'ExitCode' is not an 'IOError', 'exitWith' bypasses
-- the error handling in the 'IO' monad and cannot be intercepted by
-- 'catch' from the "Prelude". However it is a 'Control.Exception.SomeException', and can
-- be caught using the functions of "Control.Exception". This means
-- that cleanup computations added with 'Control.Exception.bracket'
-- (from "Control.Exception") are also executed properly on 'exitWith'.
--
-- Note: in GHC, 'exitWith' should be called from the main program
-- thread in order to exit the process. When called from another
-- thread, 'exitWith' will throw an 'ExitException' as normal, but the
-- exception will not cause the process itself to exit.
--
-- @since 0.1
exitWith :: (MonadThrow m) => ExitCode -> m a
exitWith ExitSuccess = SafeEx.throwM ExitSuccess
exitWith code@(ExitFailure n)
  | n /= 0 = SafeEx.throwM code
  | otherwise =
      SafeEx.throwM
        ( IOError
            Nothing
            InvalidArgument
            "exitWith"
            "ExitFailure 0"
            Nothing
            Nothing
        )
{-# INLINEABLE exitWith #-}

-- $masking
-- This section directly reexports @exceptions@' functions: we favor its
-- 'mask' over @safe-exceptions@' 'uninterruptibleMask'.
