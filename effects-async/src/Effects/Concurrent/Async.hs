{-# LANGUAGE PostfixOperators #-}

-- | Provides the 'MonadAsync' typeclass for async effects.
--
-- @since 0.1
module Effects.Concurrent.Async
  ( -- * Effect
    Async,
    MonadAsync (..),

    -- * Querying Asyncs
    wait,
    poll,
    waitCatch,
    cancel,
    uninterruptibleCancel,
    cancelWith,
    Async.asyncThreadId,

    -- * STM Operations
    STM,
    Async.waitSTM,
    Async.pollSTM,
    Async.waitCatchSTM,

    -- * Waiting for multiple Asyncs
    waitAny,
    waitAnyCatch,
    waitAnyCancel,
    waitAnyCatchCancel,
    waitEither,
    waitEitherCatch,
    waitEitherCancel,
    waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- * Waiting for multiple Asyncs in STM
    Async.waitAnySTM,
    Async.waitAnyCatchSTM,
    Async.waitEitherSTM,
    Async.waitEitherCatchSTM,
    Async.waitEitherSTM_,
    Async.waitBothSTM,

    -- * Linking
    Async.ExceptionInLinkedThread (..),
    link,
    link2,

    -- * Pooled concurrency
    -- $pool
    pooledMapConcurrentlyN,
    pooledMapConcurrently,
    pooledMapConcurrentlyN_,
    pooledMapConcurrently_,
    pooledForConcurrentlyN,
    pooledForConcurrently,
    pooledForConcurrentlyN_,
    pooledForConcurrently_,
    pooledReplicateConcurrentlyN,
    pooledReplicateConcurrently,
    pooledReplicateConcurrentlyN_,
    pooledReplicateConcurrently_,

    -- * Convenient utilities
    race_,
    mapConcurrently,
    forConcurrently,
    mapConcurrently_,
    forConcurrently_,
    replicateConcurrently,
    replicateConcurrently_,
    Concurrently,

    -- * Reexports
    SomeException,

    -- ** Positive
    -- $positive
    Positive (MkPositive),
    Positive.mkPositive,
    (+!),
  )
where

import Control.Applicative (Alternative (..), Applicative (liftA2))
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (STM)
import Control.Exception.Base
  ( BlockedIndefinitelyOnSTM
      ( BlockedIndefinitelyOnSTM
      ),
  )
import Control.Monad (forever, replicateM)
import Control.Monad.Catch
  ( Exception (fromException),
    MonadCatch,
    SomeException,
  )
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, mapReaderT)
import Data.Foldable (Foldable (fold), toList)
import Data.Functor (void, ($>))
import Data.Traversable (for)
import Effects.Concurrent.STM (MonadSTM (..))
import Effects.Concurrent.Thread
  ( MonadThread (getNumCapabilities, threadDelay, throwTo),
  )
import Effects.Exception (MonadMask, addCS, toAsyncException)
import Effects.IORef
  ( IORef,
    MonadIORef
      ( atomicModifyIORef',
        atomicWriteIORef,
        newIORef,
        readIORef
      ),
  )
import GHC.Stack (HasCallStack)
import Numeric.Data.Positive (Positive (MkPositive), (+!))
import Numeric.Data.Positive qualified as Positive

-- | Represents async effects. API largely follows
-- [unliftio](https://hackage.haskell.org/package/unliftio)'s implementation
-- of [UnliftIO.Async](https://hackage.haskell.org/package/unliftio/docs/UnliftIO-Async.html).
--
-- We prefer to implement as much of the API outside of the typeclass as
-- possible, to reduce the overall size for both ease of use and
-- performance. Nevertheless, there are many functions on the typeclass due to
-- their implementation in
-- [Control.Concurrent.Async](https://hackage.haskell.org/package/async/docs/Control-Concurrent-Async.html)
-- being complex, and we do not want to reimplement any complex logic here.
--
-- @since 0.1
class (Monad m) => MonadAsync m where
  -- | Spawn an asynchronous action in a separate thread.
  --
  -- Like for 'forkIO', the action may be left running unintentinally
  -- (see module-level documentation for details).
  --
  -- __Use 'withAsync' style functions wherever you can instead!__
  --
  -- @since 0.1
  async :: (HasCallStack) => m a -> m (Async a)

  -- | Like 'async' but using 'forkOS' internally.
  --
  -- @since 0.1
  asyncBound :: (HasCallStack) => m a -> m (Async a)

  -- | Like 'async' but using 'forkOn' internally.
  --
  -- @since 0.1
  asyncOn :: (HasCallStack) => Int -> m a -> m (Async a)

  -- | Like 'async' but using 'forkIOWithUnmask' internally. The child
  -- thread is passed a function that can be used to unmask asynchronous
  -- exceptions.
  --
  -- @since 0.1
  asyncWithUnmask :: (HasCallStack) => ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Like 'asyncOn' but using 'forkOnWithUnmask' internally. The
  -- child thread is passed a function that can be used to unmask
  -- asynchronous exceptions.
  --
  -- @since 0.1
  asyncOnWithUnmask :: (HasCallStack) => Int -> ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Spawn an asynchronous action in a separate thread, and pass its
  -- @Async@ handle to the supplied function. When the function returns
  -- or throws an exception, 'uninterruptibleCancel' is called on the @Async@.
  --
  -- > withAsync action inner = mask $ \restore -> do
  -- >   a <- async (restore action)
  -- >   restore (inner a) `finally` uninterruptibleCancel a
  --
  -- This is a useful variant of 'async' that ensures an @Async@ is
  -- never left running unintentionally.
  --
  -- Note: a reference to the child thread is kept alive until the call
  -- to `withAsync` returns, so nesting many `withAsync` calls requires
  -- linear memory.
  --
  -- @since 0.1
  withAsync :: (HasCallStack) => m a -> (Async a -> m b) -> m b

  -- | Like 'withAsync' but uses 'forkOS' internally.
  --
  -- @since 0.1
  withAsyncBound :: (HasCallStack) => m a -> (Async a -> m b) -> m b

  -- | Like 'withAsync' but uses 'forkOn' internally.
  --
  -- @since 0.1
  withAsyncOn :: (HasCallStack) => Int -> m a -> (Async a -> m b) -> m b

  -- | Like 'withAsync' but uses 'forkIOWithUnmask' internally. The
  -- child thread is passed a function that can be used to unmask
  -- asynchronous exceptions.
  --
  -- @since 0.1
  withAsyncWithUnmask ::
    (HasCallStack) =>
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Like 'withAsyncOn' but uses 'forkOnWithUnmask' internally. The
  -- child thread is passed a function that can be used to unmask
  -- asynchronous exceptions.
  --
  -- @since 0.1
  withAsyncOnWithUnmask ::
    (HasCallStack) =>
    Int ->
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Link the given @Async@ to the current thread, such that if the
  -- @Async@ raises an exception, that exception will be re-thrown in
  -- the current thread, wrapped in 'ExceptionInLinkedThread'.
  --
  -- The supplied predicate determines which exceptions in the target
  -- thread should be propagated to the source thread.
  --
  -- @since 0.1
  linkOnly :: (HasCallStack) => (SomeException -> Bool) -> Async a -> m ()

  -- | Link two @Async@s together, such that if either raises an
  -- exception, the same exception is re-thrown in the other @Async@,
  -- wrapped in 'ExceptionInLinkedThread'.
  --
  -- The supplied predicate determines which exceptions in the target
  -- thread should be propagated to the source thread.
  --
  -- @since 0.1
  link2Only :: (HasCallStack) => (SomeException -> Bool) -> Async a -> Async b -> m ()

  -- | Run two @IO@ actions concurrently, and return the first to
  -- finish. The loser of the race is 'cancel'led.
  --
  -- > race left right =
  -- >   withAsync left $ \a ->
  -- >   withAsync right $ \b ->
  -- >   waitEither a b
  --
  --
  -- @since 0.1
  race :: (HasCallStack) => m a -> m b -> m (Either a b)

  -- | Run two @IO@ actions concurrently, and return both results. If
  -- either action throws an exception at any time, then the other
  -- action is 'cancel'led, and the exception is re-thrown by
  -- 'concurrently'.
  --
  -- > concurrently left right =
  -- >   withAsync left $ \a ->
  -- >   withAsync right $ \b ->
  -- >   waitBoth a b
  --
  -- @since 0.1
  concurrently :: (HasCallStack) => m a -> m b -> m (a, b)

  -- | 'concurrently', but ignore the result values
  --
  -- @since 0.1
  concurrently_ :: (HasCallStack) => m a -> m b -> m ()

-- | @since 0.1
instance MonadAsync IO where
  async = addCS . Async.async
  {-# INLINEABLE async #-}
  asyncBound = addCS . Async.asyncBound
  {-# INLINEABLE asyncBound #-}
  asyncOn i = addCS . Async.asyncOn i
  {-# INLINEABLE asyncOn #-}
  asyncWithUnmask f = addCS $ Async.asyncWithUnmask f
  {-# INLINEABLE asyncWithUnmask #-}
  asyncOnWithUnmask i f = addCS $ Async.asyncOnWithUnmask i f
  {-# INLINEABLE asyncOnWithUnmask #-}
  withAsync m = addCS . Async.withAsync m
  {-# INLINEABLE withAsync #-}
  withAsyncBound m = addCS . Async.withAsyncBound m
  {-# INLINEABLE withAsyncBound #-}
  withAsyncOn i m = addCS . Async.withAsyncOn i m
  {-# INLINEABLE withAsyncOn #-}
  withAsyncWithUnmask f = addCS . Async.withAsyncWithUnmask f
  {-# INLINEABLE withAsyncWithUnmask #-}
  withAsyncOnWithUnmask i f = addCS . Async.withAsyncOnWithUnmask i f
  {-# INLINEABLE withAsyncOnWithUnmask #-}
  linkOnly f = addCS . Async.linkOnly f
  {-# INLINEABLE linkOnly #-}
  link2Only f x = addCS . Async.link2Only f x
  {-# INLINEABLE link2Only #-}
  race x = addCS . Async.race x
  {-# INLINEABLE race #-}
  concurrently x = addCS . Async.concurrently x
  {-# INLINEABLE concurrently #-}
  concurrently_ x = addCS . Async.concurrently_ x
  {-# INLINEABLE concurrently_ #-}

-- | @since 0.1
instance forall m env. (MonadAsync m) => MonadAsync (ReaderT env m) where
  async = mapReaderT async
  {-# INLINEABLE async #-}
  asyncBound = mapReaderT asyncBound
  {-# INLINEABLE asyncBound #-}
  asyncOn i = mapReaderT $ asyncOn i
  {-# INLINEABLE asyncOn #-}
  asyncWithUnmask m =
    ask >>= \e ->
      lift $ asyncWithUnmask $ \unmask ->
        usingReaderT e $ m $ \r -> lift (unmask (runReaderT r e))
  {-# INLINEABLE asyncWithUnmask #-}
  asyncOnWithUnmask i m =
    ask >>= \e ->
      lift $ asyncOnWithUnmask i $ \unmask ->
        usingReaderT e $ m $ \r -> lift (unmask (runReaderT r e))
  {-# INLINEABLE asyncOnWithUnmask #-}
  withAsync rdr onAsync =
    ask >>= \e ->
      lift $ withAsync (runReaderT rdr e) $ \a -> usingReaderT e (onAsync a)
  {-# INLINEABLE withAsync #-}
  withAsyncBound rdr onAsync =
    ask >>= \e ->
      lift $ withAsyncBound (runReaderT rdr e) $ \a -> usingReaderT e (onAsync a)
  {-# INLINEABLE withAsyncBound #-}
  withAsyncOn i rdr onAsync =
    ask >>= \e ->
      lift $ withAsyncOn i (runReaderT rdr e) $ usingReaderT e . onAsync
  {-# INLINEABLE withAsyncOn #-}
  withAsyncWithUnmask m onAsync =
    ask >>= \e ->
      lift $
        withAsyncWithUnmask
          (\unmask -> usingReaderT e $ m $ \r -> lift (unmask (runReaderT r e)))
          (usingReaderT e . onAsync)
  {-# INLINEABLE withAsyncWithUnmask #-}
  withAsyncOnWithUnmask i m onAsync =
    ask >>= \e ->
      lift $
        withAsyncOnWithUnmask
          i
          (\unmask -> usingReaderT e $ m $ \r -> lift (unmask (runReaderT r e)))
          (usingReaderT e . onAsync)
  {-# INLINEABLE withAsyncOnWithUnmask #-}
  linkOnly f = lift . linkOnly f
  {-# INLINEABLE linkOnly #-}
  link2Only f x = lift . link2Only f x
  {-# INLINEABLE link2Only #-}
  race left right =
    ask >>= \e ->
      let left' = runReaderT left e
          right' = runReaderT right e
       in lift $ race left' right'
  {-# INLINEABLE race #-}
  concurrently left right =
    ask >>= \e ->
      let left' = runReaderT left e
          right' = runReaderT right e
       in lift $ concurrently left' right'
  {-# INLINEABLE concurrently #-}
  concurrently_ left right =
    ask >>= \e ->
      let left' = runReaderT left e
          right' = runReaderT right e
       in lift $ concurrently_ left' right'
  {-# INLINEABLE concurrently_ #-}

usingReaderT :: forall m env a. env -> ReaderT env m a -> m a
usingReaderT = flip runReaderT
{-# INLINEABLE usingReaderT #-}

-- NOTE: Reimplemented Async functions because:
--
-- 1. Impl is simple (i.e. simple combinators)
-- 2. Reduces typeclass size
--
-- We copy INLINE pragmas, adding INLINEABLE where there are none.

-- HACK: You will see some haddock on arguments like
--
--   -- | .
--
-- This is a workaround to make haddock use 'multiline' mode as the lines
-- get really long, even though we don't really want to annotate that
-- specific field.

-- NOTE: We are using exceptions here instead of effects-exceptions for
-- everything because the real async library uses the former (actually just
-- base's Control.Exception), and we do not want to deviate from its
-- behavior.

-- | Check whether an 'Async' has completed yet. If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > poll = atomically . pollSTM
--
-- @since 0.1
poll :: forall m a. (MonadSTM m) => Async a -> m (Maybe (Either SomeException a))
poll = atomically . Async.pollSTM
{-# INLINE poll #-}

-- | Wait for an asynchronous action to complete, and return its
-- value. If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
-- @since 0.1
wait :: forall m a. (MonadCatch m, MonadSTM m) => Async a -> m a
wait = tryAgain . atomically . Async.waitSTM
{-# INLINE wait #-}

-- | Cancel an asynchronous action by throwing the @AsyncCancelled@
-- exception to it, and waiting for the `Async` thread to quit.
-- Has no effect if the 'Async' has already completed.
--
-- > cancel a = throwTo (asyncThreadId a) AsyncCancelled <* waitCatch a
--
-- Note that 'cancel' will not terminate until the thread the 'Async'
-- refers to has terminated. This means that 'cancel' will block for
-- as long said thread blocks when receiving an asynchronous exception.
--
-- For example, it could block if:
--
-- * It's executing a foreign call, and thus cannot receive the asynchronous
-- exception;
-- * It's executing some cleanup handler after having received the exception,
-- and the handler is blocking.
--
-- @since 0.1
cancel ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  Async a ->
  m ()
cancel a = throwTo (Async.asyncThreadId a) Async.AsyncCancelled <* waitCatch a
{-# INLINE cancel #-}

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
-- > cancelWith a x = throwTo (asyncThreadId a) x
--
-- The notes about the synchronous nature of 'cancel' also apply to
-- 'cancelWith'.
--
-- @since 0.1
cancelWith ::
  forall m e a.
  ( Exception e,
    HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  Async a ->
  e ->
  m ()
cancelWith a e =
  throwTo
    (Async.asyncThreadId a)
    (toAsyncException e)
    <* waitCatch a
{-# INLINEABLE cancelWith #-}

-- | Wait for any of the supplied @Async@s to complete. If the first
-- to complete throws an exception, then that exception is re-thrown
-- by 'waitAny'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
-- @since 0.1
waitAny :: forall m a. (HasCallStack, MonadSTM m) => [Async a] -> m (Async a, a)
waitAny = atomically . Async.waitAnySTM
{-# INLINE waitAny #-}

-- | Cancel an asynchronous action.
--
-- This is a variant of `cancel`, but it is not interruptible.
--
-- @since 0.1
uninterruptibleCancel ::
  forall m a.
  ( MonadAsync m,
    MonadMask m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  Async a ->
  m ()
uninterruptibleCancel = Ex.uninterruptibleMask_ . cancel
{-# INLINE uninterruptibleCancel #-}

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
--
-- @since 0.1
waitCatch ::
  forall m a.
  ( HasCallStack,
    MonadCatch m,
    MonadSTM m
  ) =>
  -- | .
  Async a ->
  m (Either SomeException a)
waitCatch = tryAgain . atomically . Async.waitCatchSTM
{-# INLINE waitCatch #-}

tryAgain :: forall m a. (MonadCatch m) => m a -> m a
tryAgain f = f `Ex.catch` \BlockedIndefinitelyOnSTM -> f
{-# INLINE tryAgain #-}

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
-- @since 0.1
waitAnyCatch ::
  forall m a.
  ( HasCallStack,
    MonadSTM m
  ) =>
  -- | .
  [Async a] ->
  m (Async a, Either SomeException a)
waitAnyCatch = atomically . Async.waitAnyCatchSTM
{-# INLINE waitAnyCatch #-}

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
-- @since 0.1
waitAnyCancel ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadMask m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  [Async a] ->
  m (Async a, a)
waitAnyCancel asyncs =
  waitAny asyncs `Ex.finally` mapM_ cancel asyncs
{-# INLINEABLE waitAnyCancel #-}

-- | Like 'waitAnyCatch', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
-- @since 0.1
waitAnyCatchCancel ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadMask m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  [Async a] ->
  m (Async a, Either SomeException a)
waitAnyCatchCancel asyncs =
  waitAnyCatch asyncs `Ex.finally` mapM_ cancel asyncs
{-# INLINEABLE waitAnyCatchCancel #-}

-- | Wait for the first of two @Async@s to finish. If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEither'.
--
-- @since 0.1
waitEither ::
  forall m a b.
  ( HasCallStack,
    MonadSTM m
  ) =>
  Async a ->
  Async b ->
  m (Either a b)
waitEither left right = atomically (Async.waitEitherSTM left right)
{-# INLINE waitEither #-}

-- | Wait for the first of two @Async@s to finish.
--
-- @since 0.1
waitEitherCatch ::
  forall m a b.
  ( HasCallStack,
    MonadSTM m
  ) =>
  -- | .
  Async a ->
  Async b ->
  m (Either (Either SomeException a) (Either SomeException b))
waitEitherCatch left right = atomically (Async.waitEitherCatchSTM left right)
{-# INLINE waitEitherCatch #-}

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
--
-- @since 0.1
waitEitherCancel ::
  forall m a b.
  ( HasCallStack,
    MonadAsync m,
    MonadMask m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  Async a ->
  Async b ->
  m (Either a b)
waitEitherCancel left right =
  waitEither left right `Ex.finally` (cancel left *> cancel right)
{-# INLINE waitEitherCancel #-}

-- | Like 'waitEitherCatch', but also 'cancel's both @Async@s before
-- returning.
--
-- @since 0.1
waitEitherCatchCancel ::
  forall m a b.
  ( HasCallStack,
    MonadAsync m,
    MonadMask m,
    MonadSTM m,
    MonadThread m
  ) =>
  -- | .
  Async a ->
  Async b ->
  m
    ( Either
        (Either SomeException a)
        (Either SomeException b)
    )
waitEitherCatchCancel left right =
  waitEitherCatch left right `Ex.finally` (cancel left *> cancel right)
{-# INLINEABLE waitEitherCatchCancel #-}

-- | Like 'waitEither', but the result is ignored.
--
-- @since 0.1
waitEither_ ::
  forall m a b.
  ( HasCallStack,
    MonadSTM m
  ) =>
  Async a ->
  Async b ->
  m ()
waitEither_ left right = atomically (Async.waitEitherSTM_ left right)
{-# INLINE waitEither_ #-}

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBoth'.
--
-- @since 0.1
waitBoth ::
  forall m a b.
  ( HasCallStack,
    MonadCatch m,
    MonadSTM m
  ) =>
  -- | .
  Async a ->
  Async b ->
  m (a, b)
waitBoth left right = tryAgain $ atomically (Async.waitBothSTM left right)
{-# INLINE waitBoth #-}

-- | Like 'race', but the result is ignored.
--
-- @since 0.1
race_ :: forall m a b. (MonadAsync m) => m a -> m b -> m ()
race_ left = void . race left
{-# INLINEABLE race_ #-}

-- | A value of type @Concurrently a@ is an @m@ operation that can be
-- composed with other @Concurrently@ values, using the @Applicative@
-- and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently a@ will
-- execute the @IO@ operations it contains concurrently, before
-- delivering the result of type @a@.
--
-- For example
--
-- > (page1, page2, page3)
-- >     <- runConcurrently $ (,,)
-- >     <$> Concurrently (getURL "url1")
-- >     <*> Concurrently (getURL "url2")
-- >     <*> Concurrently (getURL "url3")
--
-- @since 0.1
newtype Concurrently m a = Concurrently
  { -- | @since 0.1
    runConcurrently :: m a
  }

-- | @since 0.1
instance (Functor m) => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

-- | @since 0.1
instance (MonadAsync m) => Applicative (Concurrently m) where
  pure = Concurrently . pure
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

-- | @since 0.1
instance (MonadAsync m, MonadThread m) => Alternative (Concurrently m) where
  empty = Concurrently $ forever (threadDelay maxBound)
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

-- | @since 0.1
instance (MonadAsync m, Semigroup a) => Semigroup (Concurrently m a) where
  (<>) = liftA2 (<>)

-- | @since 0.1
instance (MonadAsync m, Monoid a) => Monoid (Concurrently m a) where
  mempty = pure mempty
  mappend = (<>)

-- | Maps an @m@-performing function over any 'Traversable' data
-- type, performing all the @m@ actions concurrently, and returning
-- the original data structure with the arguments replaced by the
-- results.
--
-- If any of the actions throw an exception, then all other actions are
-- cancelled and the exception is re-thrown.
--
-- For example, @mapConcurrently@ works with lists:
--
-- > pages <- mapConcurrently getURL ["url1", "url2", "url3"]
--
-- Take into account that @async@ will try to immediately spawn a thread
-- for each element of the @Traversable@, so running this on large
-- inputs without care may lead to resource exhaustion (of memory,
-- file descriptors, or other limited resources).
--
-- @since 0.1
mapConcurrently ::
  forall m t a b.
  ( MonadAsync m,
    Traversable t
  ) =>
  (a -> m b) ->
  t a ->
  m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)
{-# INLINEABLE mapConcurrently #-}

-- | `forConcurrently` is `mapConcurrently` with its arguments flipped.
--
-- > pages <- forConcurrently ["url1", "url2", "url3"] $ \url -> getURL url
--
-- @since 0.1
forConcurrently ::
  forall m t a b.
  ( MonadAsync m,
    Traversable t
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
forConcurrently = flip mapConcurrently
{-# INLINEABLE forConcurrently #-}

-- | `mapConcurrently_` is `mapConcurrently` with the return value discarded;
-- a concurrent equivalent of 'mapM_'.
--
-- @since 0.1
mapConcurrently_ ::
  forall m f a b.
  ( MonadAsync m,
    Foldable f
  ) =>
  (a -> m b) ->
  f a ->
  m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)
{-# INLINEABLE mapConcurrently_ #-}

-- | `forConcurrently_` is `forConcurrently` with the return value discarded;
-- a concurrent equivalent of 'forM_'.
--
-- @since 0.1
forConcurrently_ ::
  forall m f a b.
  ( MonadAsync m,
    Foldable f
  ) =>
  f a ->
  (a -> m b) ->
  m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINEABLE forConcurrently_ #-}

-- | Perform the action in the given number of threads.
--
-- @since 0.1
replicateConcurrently :: forall m a. (MonadAsync m) => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently . replicateM cnt . Concurrently
{-# INLINEABLE replicateConcurrently #-}

-- | Same as 'replicateConcurrently', but ignore the results.
--
-- @since 0.1
replicateConcurrently_ :: forall m a. (MonadAsync m) => Int -> m a -> m ()
replicateConcurrently_ cnt =
  runConcurrently . fold . replicate cnt . Concurrently . void
{-# INLINEABLE replicateConcurrently_ #-}

-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread, wrapped in 'ExceptionInLinkedThread'.
--
-- 'link' ignores 'AsyncCancelled' exceptions thrown in the other thread,
-- so that it's safe to 'cancel' a thread you're linked to. If you want
-- different behaviour, use 'linkOnly'.
--
-- @since 0.1
link :: (HasCallStack, MonadAsync m) => Async a -> m ()
link = linkOnly (not . isCancel)
{-# INLINEABLE link #-}

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@,
-- wrapped in 'ExceptionInLinkedThread'.
--
-- 'link2' ignores 'AsyncCancelled' exceptions, so that it's possible
-- to 'cancel' either thread without cancelling the other. If you
-- want different behaviour, use 'link2Only'.
--
-- @since 0.1
link2 :: (HasCallStack, MonadAsync m) => Async a -> Async b -> m ()
link2 = link2Only (not . isCancel)
{-# INLINEABLE link2 #-}

isCancel :: SomeException -> Bool
isCancel e
  | Just Async.AsyncCancelled <- fromException e = True
  | otherwise = False

-- NOTE: Pooled functions copied from unliftio.

-- $pool
-- These functions mirror those defined in
-- [unliftio](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#g:9).

-- | Similar to 'pooledMapConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.1
pooledMapConcurrently ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    MonadThread m,
    Traversable t
  ) =>
  -- | .
  (a -> m b) ->
  t a ->
  m (t b)
pooledMapConcurrently f xs = do
  numProcs <- getNumCapabilities
  pooledMapConcurrentlyN (numProcs +!) f xs
{-# INLINEABLE pooledMapConcurrently #-}

-- | Like 'pooledMapConcurrently' but with the return value discarded.
--
-- @since 0.1
pooledMapConcurrently_ ::
  forall m f a b.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | .
  (a -> m b) ->
  f a ->
  m ()
pooledMapConcurrently_ f t = do
  numProcs <- getNumCapabilities
  pooledMapConcurrentlyN_ (numProcs +!) f t
{-# INLINEABLE pooledMapConcurrently_ #-}

-- | Similar to 'pooledMapConcurrentlyN' but with flipped arguments.
--
-- @since 0.1
pooledForConcurrentlyN ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    Traversable t
  ) =>
  -- | Max threads > 0
  Positive Int ->
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrentlyN numProcs = flip (pooledMapConcurrentlyN numProcs)
{-# INLINEABLE pooledForConcurrentlyN #-}

-- | Similar to 'pooledForConcurrentlyN' but with number of threads
-- set from 'getNumCapabilities'. Usually this is useful for CPU bound
-- tasks.
--
-- @since 0.1
pooledForConcurrently ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    MonadThread m,
    Traversable t
  ) =>
  -- | .
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrently = flip pooledMapConcurrently
{-# INLINEABLE pooledForConcurrently #-}

-- | Like 'pooledMapConcurrentlyN_' but with flipped arguments.
--
-- @since 0.1
pooledForConcurrentlyN_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m,
    MonadIORef m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrentlyN_ numProcs = flip (pooledMapConcurrentlyN_ numProcs)
{-# INLINEABLE pooledForConcurrentlyN_ #-}

-- | Like 'pooledMapConcurrently_' but with flipped arguments.
--
-- @since 0.1
pooledForConcurrently_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | .
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrently_ = flip pooledMapConcurrently_
{-# INLINEABLE pooledForConcurrently_ #-}

-- | Pooled version of 'replicateConcurrently'. Performs the action in
-- the pooled threads.
--
-- @since 0.1
pooledReplicateConcurrentlyN ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m [a]
pooledReplicateConcurrentlyN numProcs cnt task =
  if cnt < 1
    then pure []
    else pooledMapConcurrentlyN numProcs (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrentlyN #-}

-- | Similar to 'pooledReplicateConcurrentlyN' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.1
pooledReplicateConcurrently ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | Number of times to perform the action.
  Positive Int ->
  m a ->
  m [a]
pooledReplicateConcurrently (MkPositive cnt) task =
  if cnt < 1
    then pure []
    else pooledMapConcurrently (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrently #-}

-- | Pooled version of 'replicateConcurrently_'. Performs the action in
-- the pooled threads.
--
-- @since 0.1
pooledReplicateConcurrentlyN_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m ()
pooledReplicateConcurrentlyN_ numProcs cnt task =
  if cnt < 1
    then pure ()
    else pooledMapConcurrentlyN_ numProcs (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrentlyN_ #-}

-- | Similar to 'pooledReplicateConcurrently_' but with number of
-- threads set from 'getNumCapabilities'. Usually this is useful for
-- CPU bound tasks.
--
-- @since 0.1
pooledReplicateConcurrently_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m ()
pooledReplicateConcurrently_ cnt task =
  if cnt < 1
    then pure ()
    else pooledMapConcurrently_ (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrently_ #-}

-- | Like 'mapConcurrently' from async, but instead of one thread per
-- element, it does pooling from a set of threads. This is useful in
-- scenarios where resource consumption is bounded and for use cases
-- where too many concurrent tasks aren't allowed.
--
-- === __Example usage__
--
-- @
-- import Say
--
-- action :: Int -> IO Int
-- action n = do
--   tid <- myThreadId
--   sayString $ show tid
--   threadDelay (2 * 10^6) -- 2 seconds
--   return n
--
-- main :: IO ()
-- main = do
--   yx \<- pooledMapConcurrentlyN 5 (\\x -\> action x) [1..5]
--   print yx
-- @
--
-- On executing you can see that five threads have been spawned:
--
-- @
-- \$ ./pool
-- ThreadId 36
-- ThreadId 38
-- ThreadId 40
-- ThreadId 42
-- ThreadId 44
-- [1,2,3,4,5]
-- @
--
--
-- Let's modify the above program such that there are less threads
-- than the number of items in the list:
--
-- @
-- import Say
--
-- action :: Int -> IO Int
-- action n = do
--   tid <- myThreadId
--   sayString $ show tid
--   threadDelay (2 * 10^6) -- 2 seconds
--   return n
--
-- main :: IO ()
-- main = do
--   yx \<- pooledMapConcurrentlyN 3 (\\x -\> action x) [1..5]
--   print yx
-- @
-- On executing you can see that only three threads are active totally:
--
-- @
-- \$ ./pool
-- ThreadId 35
-- ThreadId 37
-- ThreadId 39
-- ThreadId 35
-- ThreadId 39
-- [1,2,3,4,5]
-- @
--
-- @since 0.1
pooledMapConcurrentlyN ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m,
    Traversable t
  ) =>
  -- | Max threads > 0
  Positive Int ->
  (a -> m b) ->
  t a ->
  m (t b)
pooledMapConcurrentlyN numProcs f xs = do
  -- prepare one IORef per result...
  jobs :: t (a, IORef b) <-
    for
      xs
      ( \x ->
          (x,)
            <$> newIORef
              (error "Effects.MonadAsync.pooledMapConcurrentlyN: empty IORef")
      )
  -- ...put all the inputs in a queue..
  jobsVar :: IORef [(a, IORef b)] <- newIORef (toList jobs)
  -- ...run `numProcs` threads in parallel, each
  -- of them consuming the queue and filling in
  -- the respective IORefs.
  pooledConcurrently numProcs jobsVar $ \(x, outRef) ->
    f x
      >>= atomicWriteIORef outRef -- Read all the IORefs
  for jobs (\(_, outputRef) -> readIORef outputRef)
{-# INLINEABLE pooledMapConcurrentlyN #-}

-- | Like 'pooledMapConcurrentlyN' but with the return value
-- discarded.
--
-- @since 0.1
pooledMapConcurrentlyN_ ::
  forall m f a b.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadIORef m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  (a -> m b) ->
  f a ->
  m ()
pooledMapConcurrentlyN_ numProcs f =
  pooledMapConcurrentlyN_' numProcs (\x -> f x $> ())
{-# INLINEABLE pooledMapConcurrentlyN_ #-}

pooledMapConcurrentlyN_' ::
  forall m f a.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadIORef m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  (a -> m ()) ->
  f a ->
  m ()
pooledMapConcurrentlyN_' numProcs f jobs = do
  jobsVar :: IORef [a] <- newIORef (toList jobs)
  pooledConcurrently numProcs jobsVar f
{-# INLINEABLE pooledMapConcurrentlyN_' #-}

-- | Copied from unliftio.
pooledConcurrently ::
  forall m a b.
  ( HasCallStack,
    MonadAsync m,
    MonadIORef m
  ) =>
  Positive Int ->
  IORef [a] ->
  (a -> m b) ->
  m ()
pooledConcurrently (MkPositive numProcs) jobsVar f =
  replicateConcurrently_ numProcs $
    let loop = do
          mbJob :: Maybe a <- atomicModifyIORef' jobsVar $ \case
            [] -> ([], Nothing)
            var : vars -> (vars, Just var)
          case mbJob of
            Nothing -> pure ()
            Just x -> do
              _ <- f x
              loop
     in loop
{-# INLINEABLE pooledConcurrently #-}

-- $positive
-- The 'Positive' type comes from
-- [Numeric.Data.Positive](https://tbidne.github.io/smart-math/Numeric-Data-Positive.html)
-- from the
-- [smart-math](https://github.com/tbidne/smart-math) package. See that module for
-- more functionality.
