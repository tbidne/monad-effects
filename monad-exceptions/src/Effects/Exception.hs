-- | Exception handling. The interface here combines pieces of the following
-- three libraries in a somewhat idiosyncratic way:
--
-- * [exceptions](https://hackage.haskell.org/package/exceptions)
-- * [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
-- * [annotated-exception](https://hackage.haskell.org/package/annotated-exception)
--
-- We have the following goals, hence the three libraries:
--
-- 1. Typeclass abstraction: @exceptions@'s typeclasses.
-- 2. Throw exceptions w/ 'CallStack': For now, we reuse @annotated-exception@
--    for this purpose. The call stack specific functions will be removed
--    once GHC natively supports combining exceptions and 'CallStack'
--    (tentatively GHC 9.8). See the following proposal for more information:
--    https://github.com/ghc-proposals/ghc-proposals/pull/330
--
-- 3. Throw/catch exceptions in accordance with @safe-exceptions@: That is,
--    do not throw or catch async exceptions.
--
-- 4. Masking/bracket uses 'Ex.mask', not 'Ex.uninterruptibleMask': This
--    deviation is why we depend directly on @exceptions@, instead of
--    reusing @safe-exceptions@ for both. We take the position that
--    'Ex.uninterruptibleMask' should /not/ be the default behavior, as
--    legitimate uses are rare, and for those corner cases, deadlocks are worse
--    than interruptible operations receiving async exceptions.
--
-- The API exported here is not comprehensive vis-à-vis any of the three
-- libraries, so if more functionality is required, a direct dependency
-- will be necessary.
--
-- Note that, like @safe-exceptions@, the typeclass methods for
-- 'MonadThrow' ('throwM') and 'MonadCatch' ('catch') are __not__ exported
-- here because they are overridden to prevent catching async exceptions.
-- This means that if one needs to manually write an instance for either of
-- those classes, then a dependency on @exceptions@ is required.
--
-- @since 0.1
module Effects.Exception
  ( -- * Effect
    MonadGlobalException (..),

    -- * CallStack
    throwWithCallStack,
    catchWithCallStack,
    tryWithCallStack,
    addCallStack,

    -- ** Utils
    displayCallStack,
    displayNoCallStack,

    -- * Re-exports

    -- ** Throwing (@safe-exceptions@)
    MonadThrow,
    SafeEx.throwM,

    -- ** Catching (@safe-exceptions@)
    MonadCatch,
    SafeEx.catch,
    SafeEx.catchAny,
    SafeEx.handle,
    SafeEx.handleAny,
    SafeEx.try,
    SafeEx.tryAny,
    SafeEx.Handler (..),
    SafeEx.catches,
    onException,

    -- ** Masking (@exceptions@)
    MonadMask (..),
    Ex.ExitCase (..),
    Ex.mask_,
    Ex.uninterruptibleMask_,
    Ex.bracket,
    Ex.bracket_,
    Ex.finally,
    Ex.bracketOnError,

    -- ** @annotated-exception@
    AnnotatedException (..),
    Annotation (..),

    -- ** @safe-exceptions@
    SafeEx.toSyncException,
    SafeEx.toAsyncException,
    SafeEx.isSyncException,
    SafeEx.isAsyncException,

    -- * Misc
    CallStack,
    Exception (..),
    HasCallStack,
    SomeException,
    IOException,
  )
where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Exception (IOException)
import GHC.Conc.Sync qualified as Sync
import Control.Exception.Annotated
  ( AnnotatedException (..),
    Annotation (..),
  )
import Control.Exception.Annotated qualified as Ann
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch
  ( Exception (..),
    MonadCatch (..),
    MonadMask (..),
    MonadThrow (..),
    SomeException,
  )
import Control.Monad.Catch qualified as Ex
import Data.Foldable (Foldable (foldMap'))
import Data.Typeable (cast)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    prettyCallStack,
    withFrozenCallStack,
  )

-- | Effect for global exception mechanisms.
--
-- @since 0.1
class Monad m => MonadGlobalException m where
  -- | Lifted 'Sync.setUncaughtExceptionHandler'.
  --
  -- @since 0.1
  setUncaughtExceptionHandler :: HasCallStack => (SomeException -> m ()) -> m ()

  -- | Lifted 'Sync.getUncaughtExceptionHandler'.
  --
  -- @since 0.1
  getUncaughtExceptionHandler :: HasCallStack => m (SomeException -> m ())

-- | @since 0.1
instance MonadGlobalException IO where
  setUncaughtExceptionHandler = addCallStack . Sync.setUncaughtExceptionHandler
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler = addCallStack Sync.getUncaughtExceptionHandler
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | @since 0.1
instance MonadGlobalException m => MonadGlobalException (ReaderT env m) where
  setUncaughtExceptionHandler f = ask >>= \e ->
    lift $ setUncaughtExceptionHandler (\ex -> runReaderT (f ex) e)
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler =
    ask >>= \e -> lift (runReaderT getUncaughtExceptionHandler e)
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | Alias for 'Ann.throwWithCallStack'. Will eventually be removed in favor
-- of @safe-exception@'s 'SafeEx.throwM' once GHC natively handles exceptions
-- with 'CallStack' (9.8).
--
-- @since 0.1
throwWithCallStack ::
  (Exception e, HasCallStack, MonadThrow m) => e -> m a
throwWithCallStack = Ann.throwWithCallStack

-- | Alias for 'Ann.catch'. Needed to catch exceptions thrown by
-- @throwWithCallStack@, which is used liberally by the packages in this
-- repository. Will eventually be removed in favor of @safe-exception@'s
-- 'SafeEx.catch' once GHC natively handles exceptions with 'CallStack' (9.8).
--
-- @since 0.1
catchWithCallStack ::
  (Exception e, HasCallStack, MonadCatch m) => m a -> (e -> m a) -> m a
catchWithCallStack = Ann.catch

-- | Alias for 'Ann.try'. Needed to catch exceptions thrown by
-- @throwWithCallStack@, which is used liberally by the packages in this
-- repository. Will eventually be removed in favor of @safe-exception@'s
-- 'SafeEx.catch' once GHC natively handles exceptions with 'CallStack' (9.8).
--
-- @since 0.1
tryWithCallStack ::
  (Exception e, MonadCatch m) => m a -> m (Either e a)
tryWithCallStack = Ann.try

-- | Alias for 'Ann.checkpointCallStack'. Will eventually be removed in favor
-- of @safe-exception@'s 'SafeEx.catch' once GHC natively handles exceptions
-- with 'CallStack' (9.8).
--
-- @since 0.1
addCallStack :: (HasCallStack, MonadCatch m) => m a -> m a
addCallStack = Ann.checkpointCallStack

-- | Like 'displayException', except it has extra logic that attempts to
-- display any found 'CallStack's in a pretty way.
--
-- @since 0.1
displayCallStack :: forall e. Exception e => e -> String
displayCallStack ex =
  case fromException @(AnnotatedException SomeException) (toException ex) of
    Nothing -> displayException ex
    Just (AnnotatedException anns anEx) ->
      mconcat
        [ displayException anEx,
          foldMap' (\a -> "\n" <> prettyAnn a) anns
        ]
  where
    prettyAnn :: Annotation -> String
    prettyAnn (Annotation x) = case cast x of
      Just cs -> prettyCallStack cs
      Nothing -> show x

-- | Like 'displayException', except it has specific logic to skip any
-- found 'CallStack's.
--
-- @since 0.1
displayNoCallStack :: forall e. Exception e => e -> String
displayNoCallStack ex =
  case fromException @(AnnotatedException SomeException) (toException ex) of
    Nothing -> displayException ex
    Just (AnnotatedException _ anEx) -> displayException anEx

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
-- Hence this version, which is based on @safe-exceptions@' 'SafeEx.catchAny'
-- i.e. it does not catch async async exceptions, nor does it invoke any
-- masking.
--
-- @since 0.1
onException :: (HasCallStack, MonadCatch m) => m a -> m b -> m a
onException action handler =
  withFrozenCallStack SafeEx.catchAny action (\e -> handler *> throwM e)
