-- | Exception handling. The interface here combines pieces of the following
-- two libraries in a somewhat idiosyncratic way:
--
-- * [exceptions](https://hackage.haskell.org/package/exceptions)
-- * [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
--
-- We have the following goals, hence the two libraries:
--
-- 1. Typeclass abstraction: @exceptions@'s
--    'MonadThrow'\/'MonadCatch'\/'MonadMask'.
-- 2. Throw exceptions w/ 'CallStack': For now, we use a custom type
--    'ExceptionCS' for this purpose. This functionality may be removed
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
-- The API exported here is not comprehensive vis-à-vis either of the two
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
    -- $callstack
    ExceptionCS (..),
    throwWithCS,
    catchWithCS,
    catchAnyWithCS,
    handleWithCS,
    handleAnyWithCS,
    tryWithCS,
    tryAnyWithCS,
    addCS,
    addOuterCS,

    -- ** Utils
    displayNoCS,

    -- * Re-exports

    -- ** Throwing (@safe-exceptions@)
    MonadThrow,
    SafeEx.throwM,
    SafeEx.throwString,

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

import Control.DeepSeq (NFData)
import Control.Exception (IOException)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch
  ( Exception (..),
    Handler (..),
    MonadCatch (..),
    MonadMask (..),
    MonadThrow (..),
    SomeException (..),
  )
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Set qualified as Set
import Data.Typeable (cast)
import GHC.Conc.Sync qualified as Sync
import GHC.Generics (Generic)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
    withFrozenCallStack,
  )
import GHC.Stack.Types (SrcLoc (..), fromCallSiteList, getCallStack)

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
  setUncaughtExceptionHandler = addCS . Sync.setUncaughtExceptionHandler
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler = addCS Sync.getUncaughtExceptionHandler
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- | @since 0.1
instance MonadGlobalException m => MonadGlobalException (ReaderT env m) where
  setUncaughtExceptionHandler f =
    ask >>= \e ->
      lift $ setUncaughtExceptionHandler (\ex -> runReaderT (f ex) e)
  {-# INLINEABLE setUncaughtExceptionHandler #-}

  getUncaughtExceptionHandler =
    ask >>= \e -> lift (runReaderT getUncaughtExceptionHandler e)
  {-# INLINEABLE getUncaughtExceptionHandler #-}

-- $callstack
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
-- Note that if an exception @e@ is thrown with 'throwWithCS', you can
-- no longer catch it with normal @catch \@e@, as it is now a
-- @ExceptionCS e@. You can instead use @catchWithCS \@e@.

-- | Attaches a 'CallStack' to an arbitrary exception. The 'Show' instance
-- uses 'displayException' i.e. the underlying exceptions' 'displayException'
-- and pretty-prints the 'CallStack' due to some libraries (e.g. testing)
-- defaulting to 'Show' when printing exceptions.
--
-- @since 0.1
data ExceptionCS e = MkExceptionCS e CallStack
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Functor
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Applicative ExceptionCS where
  pure x = MkExceptionCS x callStack

  MkExceptionCS f cs <*> MkExceptionCS x cs' =
    MkExceptionCS (f x) (mergeCallStack cs cs')

-- | @since 0.1
instance Monad ExceptionCS where
  MkExceptionCS x cs >>= f =
    let MkExceptionCS x' cs' = f x
     in MkExceptionCS x' (mergeCallStack cs cs')

-- | Alias for 'displayException'.
--
-- @since 0.1
instance Exception e => Show (ExceptionCS e) where
  show = displayException

-- | @since 0.1
instance Exception e => Exception (ExceptionCS e) where
  -- Converting underlying exception so that we can predictably convert to
  -- ExceptionCS SomeException
  toException (MkExceptionCS ex cs) =
    tryFlatten $ SomeException (MkExceptionCS (toException ex) cs)

  -- 1. SomeException ex
  fromException (SomeException ex)
    -- 1.1. The underlying ex is (ExceptionCS e)
    --      ==> return the ExceptionCS e
    | Just x <- cast ex = Just x
    -- 1.2 The underlying ex is (ExceptionCS SomeException) AND
    --     The SomeException can be converted to the requested e
    --     ==> return the ExceptionCS e
    | Just (MkExceptionCS (ecs :: SomeException) cs) <- cast ex,
      Just x <- SafeEx.fromException ecs =
        pure $ MkExceptionCS x cs
  -- 2. ex = SomeException
  fromException ex
    -- 2.1. The ex is convertible to the requested e
    --      ==> wrap it in an ExceptionCS
    | Just e <- SafeEx.fromException ex = Just $ pure e
    -- 2.2. We did our best
    | otherwise = Nothing

  displayException (MkExceptionCS e cs) =
    mconcat
      [ displayException e,
        "\n",
        prettyCallStack cs
      ]

flatten :: ExceptionCS (ExceptionCS e) -> ExceptionCS e
flatten (MkExceptionCS (MkExceptionCS ex old) new) =
  MkExceptionCS ex (mergeCallStack old new)

tryFlatten :: SomeException -> SomeException
tryFlatten ex =
  case fromException ex of
    Just (ex' :: ExceptionCS (ExceptionCS SomeException)) ->
      SomeException $ flatten ex'
    Nothing ->
      ex

-- | Wraps an exception in 'ExceptionCS' and rethrows. If the @e@ is
-- also an 'ExceptionCS', the callStacks are merged.
--
-- @since 0.1
throwWithCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadThrow m) =>
  e ->
  m a
throwWithCS ex =
  withFrozenCallStack $ throwM $ MkExceptionCS ex callStack
{-# INLINEABLE throwWithCS #-}

-- | Catches both @e@ and @ExceptionCS e@. The given handler is
-- wrapped in 'addCS'.
--
-- @since 0.1
catchWithCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  m a ->
  (e -> m a) ->
  m a
catchWithCS action handler =
  withFrozenCallStack $
    SafeEx.catches
      action
      [ Handler $ \ex -> addCS $ handler ex,
        -- "Forget" about the callstack unless another is raised.
        Handler $ \(MkExceptionCS ex cs) -> addOuterCS cs $ handler ex
      ]
{-# INLINEABLE catchWithCS #-}

-- | 'catchWithCS' specialized to 'SomeException'.
--
-- @since 0.1
catchAnyWithCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  m a ->
  (SomeException -> m a) ->
  m a
catchAnyWithCS = catchWithCS
{-# INLINEABLE catchAnyWithCS #-}

-- | Flipped 'catchWithCS'.
--
-- @since 0.1
handleWithCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  (e -> m a) ->
  m a ->
  m a
handleWithCS = flip catchWithCS
{-# INLINEABLE handleWithCS #-}

-- | 'handleWithCS' specialized to 'SomeException'.
--
-- @since 0.1
handleAnyWithCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  (SomeException -> m a) ->
  m a ->
  m a
handleAnyWithCS = handleWithCS
{-# INLINEABLE handleAnyWithCS #-}

-- | Try for 'catchWithCS'.
--
-- @since 0.1
tryWithCS ::
  forall m e a.
  (Exception e, MonadCatch m) =>
  m a ->
  m (Either e a)
tryWithCS m = (Right <$> m) `catchWithCS` (pure . Left)
{-# INLINEABLE tryWithCS #-}

-- | 'tryWithCS' specialized to 'SomeException'.
--
-- @since 0.1
tryAnyWithCS ::
  forall m a.
  MonadCatch m =>
  m a ->
  m (Either SomeException a)
tryAnyWithCS = tryWithCS
{-# INLINEABLE tryAnyWithCS #-}

-- | Turns any caught exceptions @e@ into an @ExceptionCS e@ with
-- attached 'CallStack' and rethrows.
--
-- @since 0.1
addCS :: forall m a. (HasCallStack, MonadCatch m) => m a -> m a
addCS m = withFrozenCallStack $ addOuterCS callStack m
{-# INLINEABLE addCS #-}

-- | Like 'addCS', except it merges the given "outer callstack" with
-- the one generated by a caught exception.
--
-- @since 0.1
addOuterCS :: forall m a. (HasCallStack, MonadCatch m) => CallStack -> m a -> m a
addOuterCS outerCS m =
  m `SafeEx.catch` \(ex :: SomeException) ->
    throwM $ case fromException ex of
      Just (MkExceptionCS (innerEx :: SomeException) innerCS) ->
        MkExceptionCS
          innerEx
          (innerCS `mergeCallStack` outerCS `mergeCallStack` callStack)
      Nothing -> MkExceptionCS ex (outerCS `mergeCallStack` callStack)
{-# INLINEABLE addOuterCS #-}

mergeCallStack :: CallStack -> CallStack -> CallStack
mergeCallStack innerCS outerCS =
  fromCallSiteList $
    fmap (fmap fromSrcLocOrd) $
      ordNub $
        fmap (fmap toSrcLocOrd) $
          getCallStack innerCS <> getCallStack outerCS

toSrcLocOrd :: SrcLoc -> ([Char], [Char], [Char], Int, Int, Int, Int)
toSrcLocOrd (SrcLoc a b c d e f g) =
  (a, b, c, d, e, f, g)

fromSrcLocOrd :: ([Char], [Char], [Char], Int, Int, Int, Int) -> SrcLoc
fromSrcLocOrd (a, b, c, d, e, f, g) =
  SrcLoc a b c d e f g

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go s (x : xs)
      | Set.member x s = go s xs
      | otherwise = x : go (Set.insert x s) xs

-- | Like 'displayException', except it has specific logic to skip any
-- found 'CallStack's.
--
-- @since 0.1
displayNoCS :: forall e. Exception e => e -> String
displayNoCS ex =
  case fromException (toException ex) of
    Nothing -> displayException ex
    Just (MkExceptionCS (ecs :: SomeException) _) -> displayException ecs

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
onException :: forall m a b. (HasCallStack, MonadCatch m) => m a -> m b -> m a
onException action handler =
  withFrozenCallStack $ SafeEx.catchAny action (\e -> handler *> throwM e)
{-# INLINEABLE onException #-}
