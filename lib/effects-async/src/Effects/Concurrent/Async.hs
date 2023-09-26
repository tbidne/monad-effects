{-# LANGUAGE CPP #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

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
    MonadThread,

    -- ** Positive
    -- $positive
    Positive (MkPositive),
    Positive.mkPositive,
    Positive.mkPositiveTH,
    Positive.unsafePositive,
    (+!),
  )
where

#if MIN_VERSION_base(4, 18, 0)
import Control.Applicative (Alternative (empty, (<|>)))
#else
import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))
#endif
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Monad (forever, replicateM)
import Control.Monad.Catch (Exception, SomeException)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, mapReaderT)
import Data.Foldable (Foldable (fold))
import Data.Functor (void)
import Effects.Concurrent.Thread (MonadThread (threadDelay))
import Effects.Exception (addCS)
import GHC.Conc (STM)
import GHC.Stack (HasCallStack)
import Numeric.Data.Positive (Positive (MkPositive), (+!))
import Numeric.Data.Positive qualified as Positive
import UnliftIO.Async qualified as UAsync

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
  -- | Lifted 'Async.withAsync'.
  --
  -- @since 0.1
  withAsync :: (HasCallStack) => m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncBound'.
  --
  -- @since 0.1
  withAsyncBound :: (HasCallStack) => m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncOn'.
  --
  -- @since 0.1
  withAsyncOn :: (HasCallStack) => Int -> m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncWithUnmask'.
  --
  -- @since 0.1
  withAsyncWithUnmask ::
    (HasCallStack) =>
    -- | .
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Lifted 'Async.withAsyncOnWithUnmask'.
  --
  -- @since 0.1
  withAsyncOnWithUnmask ::
    (HasCallStack) =>
    -- | .
    Int ->
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Lifted 'Async.wait'.
  --
  -- @since 0.1
  wait :: (HasCallStack) => Async a -> m a

  -- | Lifted 'Async.poll'.
  --
  -- @since 0.1
  poll :: (HasCallStack) => Async a -> m (Maybe (Either SomeException a))

  -- | Lifted 'Async.waitCatch'.
  --
  -- @since 0.1
  waitCatch :: (HasCallStack) => Async a -> m (Either SomeException a)

  -- | Lifted 'Async.cancel'.
  --
  -- @since 0.1
  cancel :: (HasCallStack) => Async a -> m ()

  -- | Lifted 'Async.uninterruptibleCancel'.
  --
  -- @since 0.1
  uninterruptibleCancel :: (HasCallStack) => Async a -> m ()

  -- | Lifted 'Async.cancelWith'.
  --
  -- @since 0.1
  cancelWith :: (Exception e, HasCallStack) => Async a -> e -> m ()

  -- | Lifted 'Async.race'.
  --
  -- @since 0.1
  race :: (HasCallStack) => m a -> m b -> m (Either a b)

  -- | Lifted 'Async.concurrently'.
  --
  -- @since 0.1
  concurrently :: (HasCallStack) => m a -> m b -> m (a, b)

  -- | Lifted 'Async.concurrently_'.
  --
  -- @since 0.1
  concurrently_ :: (HasCallStack) => m a -> m b -> m ()

  -- | Lifted 'Async.waitAny'.
  --
  -- @since 0.1
  waitAny :: (HasCallStack) => [Async a] -> m (Async a, a)

  -- | Lifted 'Async.waitAnyCatch'.
  --
  -- @since 0.1
  waitAnyCatch ::
    (HasCallStack) =>
    -- | .
    [Async a] ->
    m (Async a, Either SomeException a)

  -- | Lifted 'Async.waitAnyCancel'.
  --
  -- @since 0.1
  waitAnyCancel :: (HasCallStack) => [Async a] -> m (Async a, a)

  -- | Lifted 'Async.waitAnyCatchCancel'.
  --
  -- @since 0.1
  waitAnyCatchCancel ::
    (HasCallStack) =>
    -- | .
    [Async a] ->
    m (Async a, Either SomeException a)

  -- | Lifted 'Async.waitEither'.
  --
  -- @since 0.1
  waitEither :: (HasCallStack) => Async a -> Async b -> m (Either a b)

  -- | Lifted 'Async.waitEitherCatch'.
  --
  -- @since 0.1
  waitEitherCatch ::
    (HasCallStack) =>
    -- | .
    Async a ->
    Async b ->
    m
      ( Either (Either SomeException a) (Either SomeException b)
      )

  -- | Lifted 'Async.waitEitherCancel'.
  --
  -- @since 0.1
  waitEitherCancel :: (HasCallStack) => Async a -> Async b -> m (Either a b)

  -- | Lifted 'Async.waitEitherCatchCancel'.
  --
  -- @since 0.1
  waitEitherCatchCancel ::
    (HasCallStack) =>
    -- | .
    Async a ->
    Async b ->
    m
      ( Either
          (Either SomeException a)
          (Either SomeException b)
      )

  -- | Lifted 'Async.waitEither_'.
  --
  -- @since 0.1
  waitEither_ :: (HasCallStack) => Async a -> Async b -> m ()

  -- | Lifted 'Async.waitBoth'.
  --
  -- @since 0.1
  waitBoth :: (HasCallStack) => Async a -> Async b -> m (a, b)

  -- | Lifted 'Async.async'.
  --
  -- @since 0.1
  async :: (HasCallStack) => m a -> m (Async a)

  -- | Lifted 'Async.asyncBound'.
  --
  -- @since 0.1
  asyncBound :: (HasCallStack) => m a -> m (Async a)

  -- | Lifted 'Async.asyncOn'.
  --
  -- @since 0.1
  asyncOn :: (HasCallStack) => Int -> m a -> m (Async a)

  -- | Lifted 'Async.asyncWithUnmask'.
  --
  -- @since 0.1
  asyncWithUnmask :: (HasCallStack) => ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Lifted 'Async.asyncOnWithUnmask'.
  --
  -- @since 0.1
  asyncOnWithUnmask :: (HasCallStack) => Int -> ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Lifted 'Async.link'.
  --
  -- @since 0.1
  link :: (HasCallStack) => Async a -> m ()

  -- | Lifted 'Async.linkOnly'.
  --
  -- @since 0.1
  linkOnly :: (HasCallStack) => (SomeException -> Bool) -> Async a -> m ()

  -- | Lifted 'Async.link2'.
  --
  -- @since 0.1
  link2 :: (HasCallStack) => Async a -> Async b -> m ()

  -- | Lifted 'Async.link2Only'.
  --
  -- @since 0.1
  link2Only :: (HasCallStack) => (SomeException -> Bool) -> Async a -> Async b -> m ()

  -- | Lifted 'Async.pooledMapConcurrentlyN'.
  --
  -- @since 0.1
  pooledMapConcurrentlyN ::
    ( HasCallStack,
      Traversable t
    ) =>
    -- | .
    Positive Int ->
    (a -> m b) ->
    t a ->
    m (t b)

  -- | Lifted 'Async.pooledMapConcurrently'.
  --
  -- @since 0.1
  pooledMapConcurrently ::
    ( HasCallStack,
      Traversable t
    ) =>
    -- | .
    (a -> m b) ->
    t a ->
    m (t b)

  -- | Lifted 'Async.pooledMapConcurrentlyN_'.
  --
  -- @since 0.1
  pooledMapConcurrentlyN_ ::
    ( Foldable f,
      HasCallStack
    ) =>
    -- | .
    Positive Int ->
    (a -> m b) ->
    f a ->
    m ()

  -- | Lifted 'Async.pooledMapConcurrently_'.
  --
  -- @since 0.1
  pooledMapConcurrently_ ::
    ( Foldable f,
      HasCallStack
    ) =>
    -- | .
    (a -> m b) ->
    f a ->
    m ()

-- | @since 0.1
instance MonadAsync IO where
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
  wait = addCS . Async.wait
  {-# INLINEABLE wait #-}
  poll = addCS . Async.poll
  {-# INLINEABLE poll #-}
  waitCatch = addCS . Async.waitCatch
  {-# INLINEABLE waitCatch #-}
  cancel = addCS . Async.cancel
  {-# INLINEABLE cancel #-}
  uninterruptibleCancel = addCS . Async.uninterruptibleCancel
  {-# INLINEABLE uninterruptibleCancel #-}
  cancelWith a = addCS . Async.cancelWith a
  {-# INLINEABLE cancelWith #-}
  race x = addCS . Async.race x
  {-# INLINEABLE race #-}
  concurrently x = addCS . Async.concurrently x
  {-# INLINEABLE concurrently #-}
  concurrently_ x = addCS . Async.concurrently_ x
  {-# INLINEABLE concurrently_ #-}
  waitAny = addCS . Async.waitAny
  {-# INLINEABLE waitAny #-}
  waitAnyCatch = addCS . Async.waitAnyCatch
  {-# INLINEABLE waitAnyCatch #-}
  waitAnyCancel = addCS . Async.waitAnyCancel
  {-# INLINEABLE waitAnyCancel #-}
  waitAnyCatchCancel = addCS . Async.waitAnyCatchCancel
  {-# INLINEABLE waitAnyCatchCancel #-}
  waitEither x = addCS . Async.waitEither x
  {-# INLINEABLE waitEither #-}
  waitEitherCatch x = addCS . Async.waitEitherCatch x
  {-# INLINEABLE waitEitherCatch #-}
  waitEitherCancel x = addCS . Async.waitEitherCancel x
  {-# INLINEABLE waitEitherCancel #-}
  waitEitherCatchCancel x = addCS . Async.waitEitherCatchCancel x
  {-# INLINEABLE waitEitherCatchCancel #-}
  waitEither_ x = addCS . Async.waitEither_ x
  {-# INLINEABLE waitEither_ #-}
  waitBoth x = addCS . Async.waitBoth x
  {-# INLINEABLE waitBoth #-}
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
  link = addCS . Async.link
  {-# INLINEABLE link #-}
  linkOnly f = addCS . Async.linkOnly f
  {-# INLINEABLE linkOnly #-}
  link2 x = addCS . Async.link2 x
  {-# INLINEABLE link2 #-}
  link2Only f x = addCS . Async.link2Only f x
  {-# INLINEABLE link2Only #-}
  pooledMapConcurrentlyN (MkPositive i) f =
    addCS . UAsync.pooledMapConcurrentlyN i f
  {-# INLINEABLE pooledMapConcurrentlyN #-}
  pooledMapConcurrently f = addCS . UAsync.pooledMapConcurrently f
  {-# INLINEABLE pooledMapConcurrently #-}
  pooledMapConcurrentlyN_ (MkPositive i) f =
    addCS . UAsync.pooledMapConcurrentlyN_ i f
  {-# INLINEABLE pooledMapConcurrentlyN_ #-}
  pooledMapConcurrently_ f = addCS . UAsync.pooledMapConcurrently_ f
  {-# INLINEABLE pooledMapConcurrently_ #-}

-- | @since 0.1
instance (MonadAsync m) => MonadAsync (ReaderT env m) where
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
  wait = lift . wait
  {-# INLINEABLE wait #-}
  poll = lift . poll
  {-# INLINEABLE poll #-}
  waitCatch = lift . waitCatch
  {-# INLINEABLE waitCatch #-}
  cancel = lift . cancel
  {-# INLINEABLE cancel #-}
  uninterruptibleCancel = lift . uninterruptibleCancel
  {-# INLINEABLE uninterruptibleCancel #-}
  cancelWith x = lift . cancelWith x
  {-# INLINEABLE cancelWith #-}
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
  waitAny = lift . waitAny
  {-# INLINEABLE waitAny #-}
  waitAnyCatch = lift . waitAnyCatch
  {-# INLINEABLE waitAnyCatch #-}
  waitAnyCancel = lift . waitAnyCancel
  {-# INLINEABLE waitAnyCancel #-}
  waitAnyCatchCancel = lift . waitAnyCatchCancel
  {-# INLINEABLE waitAnyCatchCancel #-}
  waitEither x = lift . waitEither x
  {-# INLINEABLE waitEither #-}
  waitEitherCatch x = lift . waitEitherCatch x
  {-# INLINEABLE waitEitherCatch #-}
  waitEitherCancel x = lift . waitEitherCancel x
  {-# INLINEABLE waitEitherCancel #-}
  waitEitherCatchCancel x = lift . waitEitherCatchCancel x
  {-# INLINEABLE waitEitherCatchCancel #-}
  waitEither_ x = lift . waitEither_ x
  {-# INLINEABLE waitEither_ #-}
  waitBoth x = lift . waitBoth x
  {-# INLINEABLE waitBoth #-}
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
  link = lift . link
  {-# INLINEABLE link #-}
  linkOnly f = lift . linkOnly f
  {-# INLINEABLE linkOnly #-}
  link2 x = lift . link2 x
  {-# INLINEABLE link2 #-}
  link2Only f x = lift . link2Only f x
  {-# INLINEABLE link2Only #-}

  pooledMapConcurrentlyN i f xs =
    ask >>= \e ->
      lift $ pooledMapConcurrentlyN i (usingReaderT e . f) xs
  {-# INLINEABLE pooledMapConcurrentlyN #-}
  pooledMapConcurrently f xs =
    ask >>= \e ->
      lift $ pooledMapConcurrently (usingReaderT e . f) xs
  {-# INLINEABLE pooledMapConcurrently #-}
  pooledMapConcurrentlyN_ i f xs =
    ask >>= \e ->
      lift $ pooledMapConcurrentlyN_ i (usingReaderT e . f) xs
  {-# INLINEABLE pooledMapConcurrentlyN_ #-}
  pooledMapConcurrently_ f xs =
    ask >>= \e ->
      lift $ pooledMapConcurrently_ (usingReaderT e . f) xs
  {-# INLINEABLE pooledMapConcurrently_ #-}

usingReaderT :: forall m env a. env -> ReaderT env m a -> m a
usingReaderT = flip runReaderT
{-# INLINEABLE usingReaderT #-}

-- NOTE: To reduce maintenance burden and chance of novel bugs, we include
-- _most_ effectful functions in the typeclass, even when we could
-- potentially reimplement them here. The only functions we do _not_ include
-- in the typeclass (as a modest improvement keeping the overall size down),
-- are trivial functions (e.g. race_) whose implementation do not depend on
-- other effects (e.g. STM).

-- HACK: You will see some haddock on arguments like
--
--   -- | .
--
-- This is a workaround to make haddock use 'multiline' mode as the lines
-- get really long, even though we don't really want to annotate that
-- specific field.

-- | Lifted 'Async.race_'.
--
-- @since 0.1
race_ :: forall m a b. (MonadAsync m) => m a -> m b -> m ()
race_ left = void . race left
{-# INLINEABLE race_ #-}

-- | Lifted 'Async.Concurrently'.
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

-- | Lifted 'Async.mapConcurrently'.
--
-- @since 0.1
mapConcurrently ::
  forall m t a b.
  ( MonadAsync m,
    Traversable t
  ) =>
  -- | .
  (a -> m b) ->
  t a ->
  m (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)
{-# INLINEABLE mapConcurrently #-}

-- | Lifted 'Async.forConcurrently'.
--
-- @since 0.1
forConcurrently ::
  forall m t a b.
  ( MonadAsync m,
    Traversable t
  ) =>
  -- | .
  t a ->
  (a -> m b) ->
  m (t b)
forConcurrently = flip mapConcurrently
{-# INLINEABLE forConcurrently #-}

-- | Lifted 'Async.mapConcurrently_'.
--
-- @since 0.1
mapConcurrently_ ::
  forall m f a b.
  ( MonadAsync m,
    Foldable f
  ) =>
  -- | .
  (a -> m b) ->
  f a ->
  m ()
mapConcurrently_ f = runConcurrently . foldMap (Concurrently . void . f)
{-# INLINEABLE mapConcurrently_ #-}

-- | Lifted 'Async.forConcurrently_'.
--
-- @since 0.1
forConcurrently_ ::
  forall m f a b.
  ( MonadAsync m,
    Foldable f
  ) =>
  -- | .
  f a ->
  (a -> m b) ->
  m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINEABLE forConcurrently_ #-}

-- | Lifted 'Async.replicateConcurrently'.
--
-- @since 0.1
replicateConcurrently :: forall m a. (MonadAsync m) => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently . replicateM cnt . Concurrently
{-# INLINEABLE replicateConcurrently #-}

-- | Lifted 'Async.replicateConcurrently_'.
--
-- @since 0.1
replicateConcurrently_ :: forall m a. (MonadAsync m) => Int -> m a -> m ()
replicateConcurrently_ cnt =
  runConcurrently . fold . replicate cnt . Concurrently . void
{-# INLINEABLE replicateConcurrently_ #-}

-- $pool
-- These functions mirror those defined in
-- [unliftio](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#g:9).

-- | Lifted 'UAsync.pooledForConcurrentlyN'.
--
-- @since 0.1
pooledForConcurrentlyN ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    Traversable t
  ) =>
  -- | Max threads > 0
  Positive Int ->
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrentlyN numProcs = flip (pooledMapConcurrentlyN numProcs)
{-# INLINEABLE pooledForConcurrentlyN #-}

-- | Lifted 'UAsync.pooledForConcurrently'.
--
-- @since 0.1
pooledForConcurrently ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    Traversable t
  ) =>
  -- | .
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrently = flip pooledMapConcurrently
{-# INLINEABLE pooledForConcurrently #-}

-- | Lifted 'UAsync.pooledForConcurrentlyN_'.
--
-- @since 0.1
pooledForConcurrentlyN_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m
  ) =>
  -- | Max threads > 0
  Positive Int ->
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrentlyN_ numProcs = flip (pooledMapConcurrentlyN_ numProcs)
{-# INLINEABLE pooledForConcurrentlyN_ #-}

-- | Lifted 'UAsync.pooledForConcurrently_'.
--
-- @since 0.1
pooledForConcurrently_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m
  ) =>
  -- | .
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrently_ = flip pooledMapConcurrently_
{-# INLINEABLE pooledForConcurrently_ #-}

-- | Lifted 'UAsync.pooledReplicateConcurrentlyN'.
--
-- @since 0.1
pooledReplicateConcurrentlyN ::
  forall m a.
  ( HasCallStack,
    MonadAsync m
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

-- | Lifted 'UAsync.pooledReplicateConcurrently'.
--
-- @since 0.1
pooledReplicateConcurrently ::
  forall m a.
  ( HasCallStack,
    MonadAsync m
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

-- | Lifted 'UAsync.pooledReplicateConcurrentlyN_'.
--
-- @since 0.1
pooledReplicateConcurrentlyN_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m
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

-- | Lifted 'UAsync.pooledReplicateConcurrently_'.
--
-- @since 0.1
pooledReplicateConcurrently_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m
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

-- $positive
-- The 'Positive' type comes from
-- [Numeric.Data.Positive](https://tbidne.github.io/smart-math/Numeric-Data-Positive.html)
-- from the
-- [smart-math](https://github.com/tbidne/smart-math) package. See that module for
-- more functionality.
