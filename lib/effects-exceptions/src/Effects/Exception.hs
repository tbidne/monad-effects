{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- ORMOLU_DISABLE -}

-- see NOTE: [TypeAbstractions default extensions]

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | Exception handling. The interface here combines ideas from the following
-- three libraries in a somewhat idiosyncratic way:
--
-- * [exceptions](https://hackage.haskell.org/package/exceptions)
-- * [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
--
-- We have the following goals:
--
-- 1. Typeclass abstraction: @exceptions@'s
--    'MonadThrow'\/'MonadCatch'\/'MonadMask' hierarchy.
--
-- 2. Throw exceptions w/ 'CallStack': On base < 4.20 (GHC 9.10), we use
--    legacy implementation from "Effects.Exception.CallStack.Legacy".
--    For base >= 4.20, we use GHC's native annotation framework.
--
-- 3. Throw/catch exceptions in accordance with @safe-exceptions@: That is,
--    do not throw or catch async exceptions.
--
-- 4. Masking/bracket uses 'Ex.mask', not 'Ex.uninterruptibleMask': We take
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

#if MIN_VERSION_base(4,20,0)

    Annotation.Utils.setUncaughtExceptionDisplay,
    Annotation.Utils.setUncaughtExceptionDisplayInnerMatch,

#else

    CallStack.Legacy.setUncaughtExceptionDisplay,
    CallStack.Legacy.setUncaughtExceptionDisplayCSNoMatch,

#endif

    -- * Annotations

#if MIN_VERSION_base(4,20,0)

    -- $annotations

    -- ** Utils
    Annotation.Utils.ExceptionProxy (..),
    Annotation.Utils.displayInner,
    Annotation.Utils.displayInnerMatch,
    Annotation.Utils.displayInnerMatchHandler,

#else

    -- ** CallStack
    -- $callstack
    CallStack.Legacy.ExceptionCS (..),

    -- *** Utils
    CallStack.Legacy.ExceptionProxy (..),
    CallStack.Legacy.displayNoCS,
    CallStack.Legacy.displayCSNoMatch,
    CallStack.Legacy.displayCSNoMatchHandler,

#endif

    -- ** Legacy
    throwCS,
    catchCS,
    catchAnyCS,
    handleCS,
    handleAnyCS,
    tryCS,
    tryAnyCS,
    addCS,

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

{- ORMOLU_ENABLE -}

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
#if !MIN_VERSION_base(4,20,0)
import Effects.Exception.Annotation.CallStack.Legacy qualified as CallStack.Legacy
#endif
import Effects.Exception.Annotation.Common
  ( MonadGlobalException (getUncaughtExceptionHandler, setUncaughtExceptionHandler),
  )
#if MIN_VERSION_base(4,20,0)
import Effects.Exception.Annotation.Utils qualified as Annotation.Utils
#endif
import GHC.IO.Exception (IOErrorType (InvalidArgument), IOException (IOError))
import GHC.Stack
  ( CallStack,
    HasCallStack,
    withFrozenCallStack,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

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

-- $callstack
--
-- __IMPORTANT:__ This functionality is __deprecated__ as it has been replaced
-- by GHC's native annotation framework, introduced in base 4.20 (GHC 9.10).
-- It will still exist in "Effects.Exception.CallStack.Legacy", but it will
-- not be re-exported here.
--
-- The callstack API and implementation is heavily inspired by the excellent
-- [annotated-exception](https://hackage.haskell.org/package/annotated-exception)
-- library.
--
-- The primary difference is that @annotated-exception@ is more general and
-- comprehensive; the primary exception type allows one to tie arbitrary
-- data to its exceptions.
--
-- By comparison, the functionality here is only concerned with 'CallStack'.
-- That enables both the implementation and usage to be simpler, if you only
-- have the modest goal of obtaining exception callstacks.
--
-- Another difference is that 'ExceptionCS'\'s 'Show' instance uses
-- its 'displayException', which pretty-prints the 'CallStack'. This decision
-- is due to many functions/ibraries defaulting to 'Show', so this way is
-- more conservative if you really want stacktraces, at the cost of some
-- transparency.
--
-- Note that if an exception @e@ is thrown with 'throwCS', you can
-- no longer catch it with normal @catch \@e@, as it is now a
-- @ExceptionCS e@. You can instead use @catchCS \@e@.

-------------------------------------------------------------------------------
--                             CallStack Compat                              --
-------------------------------------------------------------------------------

-- | Legacy throw with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.throwM'.
throwCS ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  e ->
  m a
{-# INLINEABLE throwCS #-}

-- | Legacy catch with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.catch'.
catchCS ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadCatch m
  ) =>
  m a ->
  (e -> m a) ->
  m a
{-# INLINEABLE catchCS #-}

-- | Legacy catch any with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.catchAny'.
catchAnyCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  m a ->
  (SomeException -> m a) ->
  m a
{-# INLINEABLE catchAnyCS #-}

-- | Legacy handle with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.handle'.
handleCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  (e -> m a) ->
  m a ->
  m a
{-# INLINEABLE handleCS #-}

-- | Legacy handle any with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.handleAny'.
handleAnyCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  (SomeException -> m a) ->
  m a ->
  m a
{-# INLINEABLE handleAnyCS #-}

-- | Legacy try with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.try'.
tryCS ::
  forall m e a.
  (Exception e, MonadCatch m) =>
  m a ->
  m (Either e a)
{-# INLINEABLE tryCS #-}

-- | Legacy try any with callstack. For base >= 4.20 (GHC 9.10), alias for
-- 'SafeEx.tryAny'.
tryAnyCS ::
  forall m a.
  (MonadCatch m) =>
  m a ->
  m (Either SomeException a)
{-# INLINEABLE tryAnyCS #-}

-- | Legacy function for attaching a callstack to a thrown exception.
-- For base >= 4.20 (GHC 9.10), alias for 'id'.
addCS :: forall m a. (HasCallStack, MonadCatch m) => m a -> m a
{-# INLINEABLE addCS #-}

#if MIN_VERSION_base(4,20,0)

throwCS = SafeEx.throwM
{-# DEPRECATED throwCS "For base >= 4.20 (GHC 9.10), throwCS is an alias for throwM." #-}

catchCS = SafeEx.catch
{-# DEPRECATED catchCS "For base >= 4.20 (GHC 9.10), catchCS is an alias for catch." #-}

catchAnyCS = SafeEx.catchAny
{-# DEPRECATED catchAnyCS "For base >= 4.20 (GHC 9.10), catchAnyCS is an alias for catchAny." #-}

handleCS = SafeEx.handle
{-# DEPRECATED handleCS "For base >= 4.20 (GHC 9.10), handleCS is an alias for handle." #-}

handleAnyCS = SafeEx.handleAny
{-# DEPRECATED handleAnyCS "For base >= 4.20 (GHC 9.10), handleAnyCS is an alias for handleAny." #-}

tryCS = SafeEx.try
{-# DEPRECATED tryCS "For base >= 4.20 (GHC 9.10), tryCS is an alias for try." #-}

tryAnyCS = SafeEx.tryAny
{-# DEPRECATED tryAnyCS "For base >= 4.20 (GHC 9.10), tryAnyCS is an alias for tryAny." #-}

addCS = id
{-# DEPRECATED addCS "For base >= 4.20 (GHC 9.10), addCS is an alias for id." #-}

#else

throwCS = CallStack.Legacy.throwCS
{-# DEPRECATED throwCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

catchCS = CallStack.Legacy.catchCS
{-# DEPRECATED catchCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

catchAnyCS = CallStack.Legacy.catchAnyCS
{-# DEPRECATED catchAnyCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

handleCS = CallStack.Legacy.handleCS
{-# DEPRECATED handleCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

handleAnyCS = CallStack.Legacy.handleAnyCS
{-# DEPRECATED handleAnyCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

tryCS = CallStack.Legacy.tryCS
{-# DEPRECATED tryCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

tryAnyCS = CallStack.Legacy.tryAnyCS
{-# DEPRECATED tryAnyCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

addCS = CallStack.Legacy.addCS
{-# DEPRECATED addCS
  [ "The legacy CallStack exception annotations are deprecated as",
    "the functionality is subsumed in base 4.20 (GHC 9.10)."
  ]
  #-}

#endif

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
exitFailure :: (HasCallStack, MonadThrow m) => m a
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
exitWith ExitSuccess = throwCS ExitSuccess
exitWith code@(ExitFailure n)
  | n /= 0 = throwCS code
  | otherwise =
      throwCS
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
