{-# LANGUAGE CPP #-}

-- see NOTE: [TypeAbstractions default extensions]

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

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
    setUncaughtExceptionDisplay,
    setUncaughtExceptionDisplayInnerMatch,

    -- * Annotations
    -- $annotations

    -- ** Utils
    ExceptionProxy (..),
    displayInner,
    displayInnerMatch,
    displayInnerMatchIgnoreExitSuccessHandler,

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

import Control.Exception (ExceptionWithContext, someExceptionContext)
import Control.Exception.Context (displayExceptionContext)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    Handler (Handler),
    MonadCatch,
    MonadMask (generalBracket, mask, uninterruptibleMask),
    MonadThrow,
    SomeException (SomeException),
  )
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (cast)
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
  setUncaughtExceptionHandler :: (HasCallStack) => (SomeException -> m ()) -> m ()

  -- | Lifted 'Sync.getUncaughtExceptionHandler'.
  --
  -- @since 0.1
  getUncaughtExceptionHandler :: (HasCallStack) => m (SomeException -> m ())

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
--                                 CallStack                                 --
-------------------------------------------------------------------------------

-- $annotations
-- GHC 9.10 introduces a new Exception Annotation framework e.g.
--
-- - "Control.Exception"
-- - "Control.Exception.Annotation"
-- - "Control.Exception.Backtrace"
-- - "Control.Exception.Context"
--
-- We do not re-export anything related to annotations here except for some
-- utilities for display / ignoring annotations in some circumstances.

-- | Calls 'displayException' on SomeExceptions' _inner_ exception i.e. given
-- @SomeException e@ calls @displayException e@. This means we will not use
-- 'SomeException''s default annotation printing.
--
-- @since 0.1
displayInner :: forall e. (Exception e) => e -> String
displayInner = walkSomeEx . toException
  where
    walkSomeEx :: SomeException -> String
    walkSomeEx (SomeException innerEx1) = case cast innerEx1 of
      Just innerSomeEx@(SomeException _) -> walkSomeEx innerSomeEx
      Nothing -> displayException innerEx1

-- | Proxy for exception types. Used with 'displayInnerMatch'.
--
-- @since 0.1
data ExceptionProxy
  = forall e. (Exception e) => MkExceptionProxy (Proxy e)

-- | @displayInnerMatch proxies e@ attempts to convert @e@ to one of the given
-- exception types. If successful, we call 'displayException' on the result.
-- Otherwise calls 'displayException' on the original parameter.
--
-- This can be useful when we do no want to use 'SomeException''s
-- @displayException@ when e.g. we do not want to print annotations by default.
-- For instance, we can use the following to ensure we do not print callstacks
-- for specific @Ex1@ and @Ex2@:
--
-- @
-- data Ex1 = ... deriving anyclass Exception
-- data Ex2 = ... deriving anyclass Exception
--
-- let  displayEx' :: SomeException -> String
--      displayEx' =
--        displayInnerMatch
--          [ MkExceptionProxy (Proxy \@Ex1),
--            MkExceptionProxy (Proxy \@Ex2)
--          ]
--
-- -- in main
-- setUncaughtExceptionHandler displayEx'
-- @
--
-- @since 0.1
displayInnerMatch :: (Exception e) => [ExceptionProxy] -> e -> String
displayInnerMatch proxies ex =
  case Maybe.mapMaybe toMatchedDesc proxies of
    (innerExDesc : _) -> innerExDesc
    _ -> displayException ex
  where
    se = toException ex

    toMatchedDesc :: ExceptionProxy -> Maybe String
    toMatchedDesc (MkExceptionProxy @e _) = case fromException @e se of
      Just e -> Just $ displayException e
      _ -> Nothing

-- | 'setUncaughtExceptionHandler' with 'displayInnerMatchIgnoreExitSuccessHandler' i.e. calls
-- 'displayException' on any uncaught exceptions, passing the result to the
-- param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
setUncaughtExceptionDisplay ::
  ( HasCallStack,
    MonadGlobalException m
  ) =>
  (String -> m ()) ->
  m ()
setUncaughtExceptionDisplay = setUncaughtExceptionDisplayInnerMatch []
{-# INLINEABLE setUncaughtExceptionDisplay #-}

-- | Like 'setUncaughtExceptionDisplay', except we attempt to match the
-- given exception types.
--
-- @since 0.1
setUncaughtExceptionDisplayInnerMatch ::
  ( HasCallStack,
    MonadGlobalException m
  ) =>
  [ExceptionProxy] ->
  (String -> m ()) ->
  m ()
setUncaughtExceptionDisplayInnerMatch proxies =
  setUncaughtExceptionHandler
    . displayInnerMatchIgnoreExitSuccessHandler proxies
{-# INLINEABLE setUncaughtExceptionDisplayInnerMatch #-}

-- FIXME: displayInnerMatchIgnoreExitSuccessHandler is a super gross name

-- | Calls 'displayInnerMatch' on the exception, passing the result to the
-- param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
displayInnerMatchIgnoreExitSuccessHandler ::
  (Applicative f) =>
  -- | Proxies to attempt to match in 'displayInnerMatch'.
  [ExceptionProxy] ->
  -- | The handler.
  (String -> f ()) ->
  -- | The exception.
  SomeException ->
  -- | The result of the handler, or @pure ()@, if we encountered
  -- 'ExitSuccess'.
  f ()
displayInnerMatchIgnoreExitSuccessHandler
  proxies
  handler
  ex = case fromException ex of
    -- Need to handle ExitSuccess, which this does.
    Just ExitSuccess -> pure ()
    Just (ExitFailure _) -> handler $ displayInnerMatch proxies ex
    Nothing -> handler $ displayInnerMatch proxies ex
{-# INLINEABLE displayInnerMatchIgnoreExitSuccessHandler #-}

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

-- TODO: Verify that catchAny does not do anything weird with uninterruptibleMask

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
exitFailure :: (HasCallStack) => (MonadThrow m) => m a
exitFailure = exitWith (ExitFailure 1)
{-# INLINEABLE exitFailure #-}

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- successfully.
--
-- @since 0.1
exitSuccess :: (HasCallStack, MonadThrow m) => m a
exitSuccess = exitWith ExitSuccess
{-# INLINEABLE exitSuccess #-}

-- | Lifted 'System.Exit.exitWith'.
--
-- @since 0.1
exitWith :: (HasCallStack, MonadThrow m) => ExitCode -> m a
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
