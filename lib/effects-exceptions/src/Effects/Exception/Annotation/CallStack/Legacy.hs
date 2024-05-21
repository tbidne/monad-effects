{-# LANGUAGE CPP #-}

-- see NOTE: [TypeAbstractions default extensions]

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

-- | Exception handling. The interface here combines ideas from the following
-- three libraries in a somewhat idiosyncratic way:
--
-- * [annotated-exception](https://hackage.haskell.org/package/annotated-exception)
-- * [exceptions](https://hackage.haskell.org/package/exceptions)
-- * [safe-exceptions](https://hackage.haskell.org/package/safe-exceptions)
--
-- We have the following goals:
--
-- 1. Typeclass abstraction: @exceptions@'s
--    'MonadThrow'\/'MonadCatch'\/'MonadMask' hierarchy.
--
-- 2. Throw exceptions w/ 'CallStack': Inspired by @annotated-exception@, we
--    use a custom type 'ExceptionCS' for this purpose. This functionality may
--    be removed once GHC natively supports combining exceptions and
--    'CallStack' (tentatively GHC 9.8). See the following proposal for more
--    information:
--    https://github.com/ghc-proposals/ghc-proposals/pull/330
--
-- 3. Throw/catch exceptions in accordance with @safe-exceptions@: That is,
--    do not throw or catch async exceptions. We do not actually re-export
--    @safe-exceptions@ functions; the functions here are bespoke, primarily
--    to add `HasCallStack` constraints.
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
module Effects.Exception.Annotation.CallStack.Legacy
  {-# DEPRECATED
    [ "The legacy CallStack exception annotations are deprecated as",
      "the functionality is subsumed in base 4.20 (GHC 9.10)."
    ]
    #-}
  ( -- * Global mechanisms
    setUncaughtExceptionDisplay,
    setUncaughtExceptionDisplayCSNoMatch,

    -- * CallStack
    -- $callstack
    ExceptionCS (..),
    throwCS,
    catchCS,
    catchAnyCS,
    handleCS,
    handleAnyCS,
    tryCS,
    tryAnyCS,
    addCS,
    addOuterCS,

    -- ** Utils
    ExceptionProxy (..),
    displayNoCS,
    displayCSNoMatch,
    displayCSNoMatchHandler,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad.Catch
  ( Exception (displayException, fromException, toException),
    Handler (Handler),
    MonadCatch,
    MonadThrow,
    SomeException (SomeException),
  )
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Typeable (cast)
import Effects.Exception.Annotation.Common
  ( ExceptionProxy (MkExceptionProxy),
    MonadGlobalException (setUncaughtExceptionHandler),
  )
import GHC.Generics (Generic)
import GHC.Stack
  ( CallStack,
    HasCallStack,
    callStack,
    prettyCallStack,
    withFrozenCallStack,
  )
import GHC.Stack.Types (SrcLoc (SrcLoc), fromCallSiteList, getCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-------------------------------------------------------------------------------
--                                 CallStack                                 --
-------------------------------------------------------------------------------

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
-- Note that if an exception @e@ is thrown with 'throwCS', you can
-- no longer catch it with normal @catch \@e@, as it is now a
-- @ExceptionCS e@. You can instead use @catchCS \@e@.

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
instance (Exception e) => Show (ExceptionCS e) where
  show = displayException

-- | @since 0.1
instance (Exception e) => Exception (ExceptionCS e) where
  -- Converting underlying exception so that we can predictably convert to
  -- ExceptionCS SomeException
  toException (MkExceptionCS ex cs) =
    tryFlatten $ SomeException (MkExceptionCS (toException ex) cs)

  fromException someEx@(SomeException innerEx)
    -- innerEx == ExceptionCS e
    -- ==> ExceptionCS e
    | Just x <- cast innerEx = Just x
    -- innerEx == ExceptionCS innerSomeEx@SomeException
    -- innerSomeEx == e
    -- ==> ExceptionCS e
    | Just (MkExceptionCS (innerSomeEx :: SomeException) cs) <- cast innerEx,
      Just x <- fromException innerSomeEx =
        Just $ MkExceptionCS x cs
    -- SomeException == e
    -- ==> ExceptionCS e
    | Just x <- fromException someEx = Just $ pure x
    -- We did our best
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
throwCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadThrow m) =>
  e ->
  m a
throwCS ex =
  withFrozenCallStack $ SafeEx.throwM $ MkExceptionCS ex callStack
{-# INLINEABLE throwCS #-}

-- | Catches both @e@ and @ExceptionCS e@. The given handler is
-- wrapped in 'addCS'.
--
-- @since 0.1
catchCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  m a ->
  (e -> m a) ->
  m a
catchCS action handler =
  withFrozenCallStack
    SafeEx.catches
    action
    [ Handler $ \ex -> addCS $ handler ex,
      -- "Forget" about the callstack unless another is raised.
      Handler $ \(MkExceptionCS ex cs) -> addOuterCS cs $ handler ex
    ]
{-# INLINEABLE catchCS #-}

-- | 'catchCS' specialized to all synchronous exceptions.
--
-- @since 0.1
catchAnyCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  m a ->
  (SomeException -> m a) ->
  m a
catchAnyCS = catchCS
{-# INLINEABLE catchAnyCS #-}

-- | Flipped 'catchCS'.
--
-- @since 0.1
handleCS ::
  forall m e a.
  (Exception e, HasCallStack, MonadCatch m) =>
  (e -> m a) ->
  m a ->
  m a
handleCS = flip catchCS
{-# INLINEABLE handleCS #-}

-- | 'handleCS' specialized to 'SomeException'.
--
-- @since 0.1
handleAnyCS ::
  forall m a.
  (HasCallStack, MonadCatch m) =>
  (SomeException -> m a) ->
  m a ->
  m a
handleAnyCS = handleCS
{-# INLINEABLE handleAnyCS #-}

-- | Try for 'catchCS'.
--
-- @since 0.1
tryCS ::
  forall m e a.
  (Exception e, MonadCatch m) =>
  m a ->
  m (Either e a)
tryCS m = (Right <$> m) `catchCS` (pure . Left)
{-# INLINEABLE tryCS #-}

-- | 'tryCS' specialized to 'SomeException'.
--
-- @since 0.1
tryAnyCS ::
  forall m a.
  (MonadCatch m) =>
  m a ->
  m (Either SomeException a)
tryAnyCS = tryCS
{-# INLINEABLE tryAnyCS #-}

-- | Turns any caught exceptions @e@ into an @ExceptionCS e@ with
-- attached 'CallStack' and rethrows.
--
-- @since 0.1
addCS :: forall m a. (HasCallStack, MonadCatch m) => m a -> m a
addCS = withFrozenCallStack addOuterCS callStack
{-# INLINEABLE addCS #-}

-- | Like 'addCS', except it merges the given "outer callstack" with
-- the one generated by a caught exception.
--
-- @since 0.1
addOuterCS :: forall m a. (HasCallStack, MonadCatch m) => CallStack -> m a -> m a
addOuterCS outerCS m =
  m `SafeEx.catch` \(ex :: SomeException) ->
    SafeEx.throwM $ case fromException ex of
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

ordNub :: (Ord a) => [a] -> [a]
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
displayNoCS :: forall e. (Exception e) => e -> String
displayNoCS = displayCSNoMatch [someExceptionProxy]

#if MIN_VERSION_base(4, 20, 0)

{-# DEPRECATED displayNoCS
  [ "For base >= 4.20 (GHC 9.10), CallStacks as part of SomeException's ",
    "displayException are _not_ removed. Use 'displayInner' instead."
  ]
#-}

#endif

someExceptionProxy :: ExceptionProxy
someExceptionProxy = MkExceptionProxy (Proxy @SomeException)

-- | @displayCSNoMatch proxies e@ is equivalent to 'displayNoCS' if @e@
-- matches the type of some proxy in p. Otherwise calls 'displayException'.
--
-- This can be useful if we ordinarily do not want to print callstacks, doing
-- so only in an unexpected case. For instance, we can use the following to
-- ensure we do not print callstacks for specific @Ex1@ and @Ex2@:
--
-- @
-- data Ex1 = ... deriving anyclass Exception
-- data Ex2 = ... deriving anyclass Exception
--
-- let  displayEx' :: SomeException -> String
--      displayEx' =
--        displayCSNoMatch
--          [ MkExceptionProxy (Proxy \@Ex1),
--            MkExceptionProxy (Proxy \@Ex2)
--          ]
--
-- -- in main
-- setUncaughtExceptionHandler displayEx'
-- @
--
-- @since 0.1
displayCSNoMatch :: (Exception e) => [ExceptionProxy] -> e -> String
displayCSNoMatch proxies ex =
  case Maybe.mapMaybe matches proxies of
    (innerEx : _) -> displayException innerEx
    _ -> displayException ex
  where
    se = toException ex
    matches :: ExceptionProxy -> Maybe SomeException
    matches (MkExceptionProxy @e _) = case fromException se of
      Just (MkExceptionCS innerEx _) -> case fromException @e innerEx of
        Just _ -> Just innerEx
        Nothing -> Nothing
      Nothing -> Nothing

#if MIN_VERSION_base(4, 20, 0)

{-# DEPRECATED displayCSNoMatch
  [ "For base >= 4.20 (GHC 9.10), CallStacks as part of SomeException's ",
    "displayException are _not_ removed. Use 'displayInner' instead."
  ]
#-}

#endif

-- | 'setUncaughtExceptionHandler' with 'displayCSNoMatchHandler' i.e. calls
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
setUncaughtExceptionDisplay = setUncaughtExceptionDisplayCSNoMatch []
{-# INLINEABLE setUncaughtExceptionDisplay #-}

-- | Like 'setUncaughtExceptionDisplay', except callstacks are not printed for
-- exceptions matching the param proxies a la 'displayCSNoMatch'.
--
-- @since 0.1
setUncaughtExceptionDisplayCSNoMatch ::
  ( HasCallStack,
    MonadGlobalException m
  ) =>
  [ExceptionProxy] ->
  (String -> m ()) ->
  m ()
setUncaughtExceptionDisplayCSNoMatch proxies =
  setUncaughtExceptionHandler
    . displayCSNoMatchHandler proxies
{-# INLINEABLE setUncaughtExceptionDisplayCSNoMatch #-}

-- | Calls 'displayCSNoMatch' on any uncaught exceptions, passing the result to
-- the param handler. 'ExitSuccess' is ignored.
--
-- @since 0.1
displayCSNoMatchHandler ::
  (Applicative f) =>
  [ExceptionProxy] ->
  (String -> f ()) ->
  SomeException ->
  f ()
displayCSNoMatchHandler proxies handler ex = case fromException ex of
  -- Need to handle ExitSuccess and MkExceptionCS ExitSuccess, which this
  -- does.
  Just (MkExceptionCS ExitSuccess _) -> pure ()
  Just (MkExceptionCS (ExitFailure _) _) -> handler $ displayCSNoMatch proxies ex
  Nothing -> handler $ displayCSNoMatch proxies ex
{-# INLINEABLE displayCSNoMatchHandler #-}
