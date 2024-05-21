{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,20,0)

{-# LANGUAGE TypeAbstractions #-}

-- | Provides utilities for working with GHC's native Exception annotations
-- for base >= 4.20 (GHC 9.10).
--
-- @since 0.1
module Effects.Exception.Annotation.Utils
  ( -- * Global mechanisms
    setUncaughtExceptionDisplay,
    setUncaughtExceptionDisplayInnerMatch,

    -- * Annotations
    -- $annotations

    -- ** Utils
    ExceptionProxy (..),
    displayInner,
    displayInnerMatch,
    displayInnerMatchHandler,
  )
where

import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    SomeException (SomeException),
  )
import Data.Maybe qualified as Maybe
import Data.Typeable (cast)
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Effects.Exception.Annotation.Common
    ( MonadGlobalException(setUncaughtExceptionHandler),
      ExceptionProxy (MkExceptionProxy)
    )

-------------------------------------------------------------------------------
--                                Annotations                                --
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

-- | 'setUncaughtExceptionHandler' with 'displayInnerMatchHandler' i.e. calls
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
    . displayInnerMatchHandler proxies
{-# INLINEABLE setUncaughtExceptionDisplayInnerMatch #-}

-- | Calls 'displayInnerMatch' on the exception, passing the result to the
-- param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
displayInnerMatchHandler ::
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
displayInnerMatchHandler
  proxies
  handler
  ex = case fromException ex of
    -- Need to handle ExitSuccess, which this does.
    Just ExitSuccess -> pure ()
    Just (ExitFailure _) -> handler $ displayInnerMatch proxies ex
    Nothing -> handler $ displayInnerMatch proxies ex
{-# INLINEABLE displayInnerMatchHandler #-}

#else

module Effects.Exception.Annotation.Utils () where

#endif
