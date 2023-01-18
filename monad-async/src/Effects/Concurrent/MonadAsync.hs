{-# LANGUAGE PostfixOperators #-}

-- | Provides the 'MonadAsync' typeclass for async effects.
--
-- @since 0.1
module Effects.Concurrent.MonadAsync
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
import Control.Exception.Safe (MonadMask)
import Control.Exception.Safe qualified as SafeEx
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
import Effects.Concurrent.MonadSTM (MonadSTM (..))
import Effects.Concurrent.MonadThread
  ( MonadThread (getNumCapabilities, threadDelay, throwTo),
  )
import Effects.MonadCallStack (MonadCallStack (addCallStack))
import Effects.MonadIORef
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
class Monad m => MonadAsync m where
  -- | Lifted 'Async.async'.
  --
  -- @since 0.1
  async :: HasCallStack => m a -> m (Async a)

  -- | Lifted 'Async.asyncBound'.
  --
  -- @since 0.1
  asyncBound :: HasCallStack => m a -> m (Async a)

  -- | Lifted 'Async.asyncOn'.
  --
  -- @since 0.1
  asyncOn :: HasCallStack => Int -> m a -> m (Async a)

  -- | Lifted 'Async.asyncWithUnmask'.
  --
  -- @since 0.1
  asyncWithUnmask :: HasCallStack => ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Lifted 'Async.asyncOnWithUnmask'.
  --
  -- @since 0.1
  asyncOnWithUnmask :: HasCallStack => Int -> ((forall b. m b -> m b) -> m a) -> m (Async a)

  -- | Lifted 'Async.withAsync'.
  --
  -- @since 0.1
  withAsync :: HasCallStack => m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncBound'.
  --
  -- @since 0.1
  withAsyncBound :: HasCallStack => m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncOn'.
  --
  -- @since 0.1
  withAsyncOn :: HasCallStack => Int -> m a -> (Async a -> m b) -> m b

  -- | Lifted 'Async.withAsyncWithUnmask'.
  --
  -- @since 0.1
  withAsyncWithUnmask ::
    HasCallStack =>
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Lifted 'Async.withAsyncOnWithUnmask'.
  --
  -- @since 0.1
  withAsyncOnWithUnmask ::
    HasCallStack =>
    Int ->
    ((forall c. m c -> m c) -> m a) ->
    (Async a -> m b) ->
    m b

  -- | Lifted 'Async.linkOnly'.
  --
  -- @since 0.1
  linkOnly :: HasCallStack => (SomeException -> Bool) -> Async a -> m ()

  -- | Lifted 'Async.link2Only'.
  --
  -- @since 0.1
  link2Only :: HasCallStack => (SomeException -> Bool) -> Async a -> Async b -> m ()

  -- | Lifted 'Async.race'.
  --
  -- @since 0.1
  race :: HasCallStack => m a -> m b -> m (Either a b)

  -- | Lifted 'Async.concurrently'.
  --
  -- @since 0.1
  concurrently :: HasCallStack => m a -> m b -> m (a, b)

  -- | Lifted 'Async.concurrently_'.
  --
  -- @since 0.1
  concurrently_ :: HasCallStack => m a -> m b -> m ()

-- | @since 0.1
instance MonadAsync IO where
  async = addCallStack . Async.async
  {-# INLINEABLE async #-}
  asyncBound = addCallStack . Async.asyncBound
  {-# INLINEABLE asyncBound #-}
  asyncOn i = addCallStack . Async.asyncOn i
  {-# INLINEABLE asyncOn #-}
  asyncWithUnmask f = addCallStack $ Async.asyncWithUnmask f
  {-# INLINEABLE asyncWithUnmask #-}
  asyncOnWithUnmask i f = addCallStack $ Async.asyncOnWithUnmask i f
  {-# INLINEABLE asyncOnWithUnmask #-}
  withAsync m = addCallStack . Async.withAsync m
  {-# INLINEABLE withAsync #-}
  withAsyncBound m = addCallStack . Async.withAsyncBound m
  {-# INLINEABLE withAsyncBound #-}
  withAsyncOn i m = addCallStack . Async.withAsyncOn i m
  {-# INLINEABLE withAsyncOn #-}
  withAsyncWithUnmask f = addCallStack . Async.withAsyncWithUnmask f
  {-# INLINEABLE withAsyncWithUnmask #-}
  withAsyncOnWithUnmask i f = addCallStack . Async.withAsyncOnWithUnmask i f
  {-# INLINEABLE withAsyncOnWithUnmask #-}
  linkOnly f = addCallStack . Async.linkOnly f
  {-# INLINEABLE linkOnly #-}
  link2Only f x = addCallStack . Async.link2Only f x
  {-# INLINEABLE link2Only #-}
  race x = addCallStack . Async.race x
  {-# INLINEABLE race #-}
  concurrently x = addCallStack . Async.concurrently x
  {-# INLINEABLE concurrently #-}
  concurrently_ x = addCallStack . Async.concurrently_ x
  {-# INLINEABLE concurrently_ #-}

-- | @since 0.1
instance forall m env. MonadAsync m => MonadAsync (ReaderT env m) where
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

-- | Lifted 'Async.poll'.
--
-- @since 0.1
poll :: forall m a. MonadSTM m => Async a -> m (Maybe (Either SomeException a))
poll = atomically . Async.pollSTM
{-# INLINE poll #-}

-- | Lifted 'Async.wait'.
--
-- @since 0.1
wait :: forall m a. (MonadCatch m, MonadSTM m) => Async a -> m a
wait = tryAgain . atomically . Async.waitSTM
{-# INLINE wait #-}

-- | Lifted 'Async.cancel'.
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

-- | Lifted 'Async.cancelWith'.
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
    (SafeEx.toAsyncException e)
    <* waitCatch a
{-# INLINEABLE cancelWith #-}

-- | Lifted 'Async.waitAny'.
--
-- @since 0.1
waitAny :: forall m a. (HasCallStack, MonadSTM m) => [Async a] -> m (Async a, a)
waitAny = atomically . Async.waitAnySTM
{-# INLINE waitAny #-}

-- | Lifted 'Async.uninterruptibleCancel'.
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

-- | Lifted 'Async.waitCatch'.
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

tryAgain :: forall m a. MonadCatch m => m a -> m a
tryAgain f = f `Ex.catch` \BlockedIndefinitelyOnSTM -> f
{-# INLINE tryAgain #-}

-- | Lifted 'Async.waitAnyCatch'.
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

-- | Lifted 'Async.waitAnyCancel'.
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

-- | Lifted 'Async.waitAnyCatchCancel'.
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

-- | Lifted 'Async.waitEither'.
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

-- | Lifted 'Async.waitEitherCatch'.
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

-- | Lifted 'Async.waitEitherCancel'.
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

-- | Lifted 'Async.waitEitherCatchCancel'.
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

-- | Lifted 'Async.waitEither_'.
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

-- | Lifted 'Async.waitBoth'.
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

-- | Lifted 'Async.race_'.
--
-- @since 0.1
race_ :: forall m a b. MonadAsync m => m a -> m b -> m ()
race_ left = void . race left
{-# INLINEABLE race_ #-}

-- | Like 'Async.Concurrently', but lifted to some @m@.
--
-- @since 0.1
newtype Concurrently m a = Concurrently
  { -- | @since 0.1
    runConcurrently :: m a
  }

-- | @since 0.1
instance Functor m => Functor (Concurrently m) where
  fmap f (Concurrently a) = Concurrently $ f <$> a

-- | @since 0.1
instance MonadAsync m => Applicative (Concurrently m) where
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
  f a ->
  (a -> m b) ->
  m ()
forConcurrently_ = flip mapConcurrently_
{-# INLINEABLE forConcurrently_ #-}

-- | Lifted 'Async.replicateConcurrently'.
--
-- @since 0.1
replicateConcurrently :: forall m a. MonadAsync m => Int -> m a -> m [a]
replicateConcurrently cnt = runConcurrently . replicateM cnt . Concurrently
{-# INLINEABLE replicateConcurrently #-}

-- | Lifted 'Async.replicateConcurrently_'.
--
-- @since 0.1
replicateConcurrently_ :: forall m a. MonadAsync m => Int -> m a -> m ()
replicateConcurrently_ cnt =
  runConcurrently . fold . replicate cnt . Concurrently . void
{-# INLINEABLE replicateConcurrently_ #-}

-- | Lifted 'Async.link'.
--
-- @since 0.1
link :: (HasCallStack, MonadAsync m) => Async a -> m ()
link = linkOnly (not . isCancel)
{-# INLINEABLE link #-}

-- | Lifted 'Async.link2'.
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

-- | @unliftio@'s [pooledMapConcurrently](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledMapConcurrently).
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

-- | @unliftio@'s [pooledMapConcurrently_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledMapConcurrently_)
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

-- | @unliftio@'s [pooledForConcurrentlyN](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledForConcurrentlyN).
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

-- | @unliftio@'s [pooledForConcurrently](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledForConcurrently).
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

-- | @unliftio@'s [pooledForConcurrentlyN_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledForConcurrentlyN_).
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

-- | @unliftio@'s [pooledForConcurrently_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledForConcurrently_).
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

-- | @unliftio@'s [pooledReplicateConcurrentlyN](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledReplicateConcurrentlyN).
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

-- | @unliftio@'s [pooledReplicateConcurrently](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledReplicateConcurrently).
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

-- | @unliftio@'s [pooledReplicateConcurrentlyN_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledReplicateConcurrentlyN_).
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

-- | @unliftio@'s [pooledReplicateConcurrently_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledReplicateConcurrently_).
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

-- | @unliftio@'s [pooledMapConcurrentlyN](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledMapConcurrentlyN).
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

-- | @unliftio@'s [pooledMapConcurrentlyN_](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledMapConcurrentlyN_).
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

-- | @unliftio@'s [pooledMapConcurrentlyN_'](https://hackage.haskell.org/package/unliftio-0.2.23.0/docs/UnliftIO-Async.html#v:pooledMapConcurrentlyN_').
--
-- @since 0.1
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
