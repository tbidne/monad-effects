{-# LANGUAGE InstanceSigs #-}

-- | Provides the 'MonadAsync' typeclass for async effects.
--
-- @since 0.1
module Effects.MonadAsync
  ( -- * Effect
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

    -- * Pooled concurrency
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
    Async,
    SomeException,
    STM,
  )
where

import Control.Applicative (Alternative (..), Applicative (liftA2))
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (STM)
import Control.Exception.Base (BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM))
import Control.Exception.Safe (MonadMask)
import Control.Exception.Safe qualified as SafeEx
import Control.Monad (forever, replicateM)
import Control.Monad.Catch (Exception, MonadCatch, SomeException)
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, mapReaderT)
import Data.Foldable (Foldable (fold), toList)
import Data.Functor (void, ($>))
import Data.Traversable (for)
import Effects.MonadCallStack (MonadCallStack (addCallStack, throwWithCallStack))
import Effects.MonadIORef
  ( IORef,
    MonadIORef
      ( atomicModifyIORef',
        atomicWriteIORef,
        newIORef,
        readIORef
      ),
  )
import Effects.MonadSTM (MonadSTM (..))
import Effects.MonadThread
  ( MonadThread (getNumCapabilities, microsleep, throwTo),
  )
import GHC.Stack (HasCallStack)

-- | Represents async effects. API largely follows @unliftio@'s implementation
-- of "UnliftIO.Async".
--
-- We prefer to implement as much of the API outside of the typeclass as
-- possible, to reduce the overall size for both ease of use and
-- performance. Nevertheless, there are many functions on the typeclass due to
-- their implementation in "Control.Concurrent.Async" being complex, and we
-- do not want to reimplement any complex logic here.
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

  -- | Lifted 'Async.link'.
  --
  -- @since 0.1
  link :: HasCallStack => Async a -> m ()

  -- | Lifted 'Async.link2'.
  --
  -- @since 0.1
  link2 :: HasCallStack => Async a -> Async b -> m ()

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
  link = addCallStack . Async.link
  {-# INLINEABLE link #-}
  link2 x = addCallStack . Async.link2 x
  {-# INLINEABLE link2 #-}
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
  asyncWithUnmask unmask =
    ask >>= \e ->
      lift $ asyncWithUnmask $ \k ->
        usingReaderT e $ unmask $ \r -> lift (k (runReaderT r e))
  {-# INLINEABLE asyncWithUnmask #-}
  asyncOnWithUnmask i unmask =
    ask >>= \e ->
      lift $ asyncOnWithUnmask i $ \k ->
        usingReaderT e $ unmask $ \r -> lift (k (runReaderT r e))
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
  withAsyncWithUnmask unmask onAsync =
    ask >>= \e ->
      lift $
        withAsyncWithUnmask
          (\k -> usingReaderT e $ unmask $ \r -> lift (k (runReaderT r e)))
          (usingReaderT e . onAsync)
  {-# INLINEABLE withAsyncWithUnmask #-}
  withAsyncOnWithUnmask i unmask onAsync =
    ask >>= \e ->
      lift $
        withAsyncOnWithUnmask
          i
          (\k -> usingReaderT e $ unmask $ \r -> lift (k (runReaderT r e)))
          (usingReaderT e . onAsync)
  {-# INLINEABLE withAsyncOnWithUnmask #-}
  link = lift . link
  {-# INLINEABLE link #-}
  link2 x = lift . link2 x
  {-# INLINEABLE link2 #-}
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
  Async a ->
  m ()
cancel a = throwTo (Async.asyncThreadId a) Async.AsyncCancelled <* waitCatch a
{-# INLINE cancel #-}

-- | Lifted 'Async.cancelWith'.
--
-- @since 0.1
cancelWith ::
  forall e m a.
  ( Exception e,
    HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadSTM m,
    MonadThread m
  ) =>
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
  [Async a] ->
  m (Async a, a)
waitAnyCancel asyncs =
  waitAny asyncs `Ex.finally` mapM_ cancel asyncs
{-# INLINEABLE waitAnyCancel #-}

--- | Lifted 'Async.waitAnyCatchCancel'.
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
  empty = Concurrently $ forever (microsleep 9223372036854775807)
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

-- NOTE: Pooled functions copied from unliftio.

-- | Exception for pooled-specific failures (i.e. threads < 0, empty IORef).
--
-- @since 0.1
newtype PooledException = MkPooledException String
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PooledException where
  displayException (MkPooledException s) = s

-- | @unliftio@'s @pooledMapConcurrently@.
--
-- @since 0.1
pooledMapConcurrently ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m,
    Traversable t
  ) =>
  (a -> m b) ->
  t a ->
  m (t b)
pooledMapConcurrently f xs = do
  numProcs <- getNumCapabilities
  pooledMapConcurrentlyN numProcs f xs
{-# INLINEABLE pooledMapConcurrently #-}

-- | @unliftio@'s @pooledMapConcurrently_@.
--
-- @since 0.1
pooledMapConcurrently_ ::
  forall m f a b.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m
  ) =>
  (a -> m b) ->
  f a ->
  m ()
pooledMapConcurrently_ f t = do
  numProcs <- getNumCapabilities
  pooledMapConcurrentlyN_ numProcs f t
{-# INLINEABLE pooledMapConcurrently_ #-}

-- | @unliftio@'s @pooledForConcurrentlyN@.
--
-- @since 0.1
pooledForConcurrentlyN ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    Traversable t
  ) =>
  Int ->
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrentlyN numProcs = flip (pooledMapConcurrentlyN numProcs)
{-# INLINEABLE pooledForConcurrentlyN #-}

-- | @unliftio@'s @pooledForConcurrently@.
--
-- @since 0.1
pooledForConcurrently ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m,
    Traversable t
  ) =>
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrently = flip pooledMapConcurrently
{-# INLINEABLE pooledForConcurrently #-}

-- | @unliftio@'s @pooledForConcurrentlyN_@.
--
-- @since 0.1
pooledForConcurrentlyN_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m
  ) =>
  Int ->
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrentlyN_ numProcs = flip (pooledMapConcurrentlyN_ numProcs)
{-# INLINEABLE pooledForConcurrentlyN_ #-}

-- | @unliftio@'s @pooledForConcurrently_@.
--
-- @since 0.1
pooledForConcurrently_ ::
  forall m f a b.
  ( Foldable f,
    HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m
  ) =>
  f a ->
  (a -> m b) ->
  m ()
pooledForConcurrently_ = flip pooledMapConcurrently_
{-# INLINEABLE pooledForConcurrently_ #-}

-- | @unliftio@'s @pooledReplicateConcurrentlyN@.
--
-- @since 0.1
pooledReplicateConcurrentlyN ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m
  ) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m [a]
pooledReplicateConcurrentlyN numProcs cnt task =
  if cnt < 1
    then return []
    else pooledMapConcurrentlyN numProcs (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrentlyN #-}

-- | @unliftio@'s @pooledReplicateConcurrently@.
--
-- @since 0.1
pooledReplicateConcurrently ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m [a]
pooledReplicateConcurrently cnt task =
  if cnt < 1
    then return []
    else pooledMapConcurrently (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrently #-}

-- | @unliftio@'s @pooledReplicateConcurrentlyN_@.
--
-- @since 0.1
pooledReplicateConcurrentlyN_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m
  ) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m ()
pooledReplicateConcurrentlyN_ numProcs cnt task =
  if cnt < 1
    then return ()
    else pooledMapConcurrentlyN_ numProcs (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrentlyN_ #-}

-- | @unliftio@'s @pooledReplicateConcurrently_@.
--
-- @since 0.1
pooledReplicateConcurrently_ ::
  forall m a.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    MonadThread m
  ) =>
  -- | Number of times to perform the action.
  Int ->
  m a ->
  m ()
pooledReplicateConcurrently_ cnt task =
  if cnt < 1
    then return ()
    else pooledMapConcurrently_ (const task) [1 .. cnt]
{-# INLINEABLE pooledReplicateConcurrently_ #-}

-- | @unliftio@'s @pooledMapConcurrentlyN@.
--
-- @since 0.1
pooledMapConcurrentlyN ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    Traversable t
  ) =>
  Int ->
  (a -> m b) ->
  t a ->
  m (t b)
pooledMapConcurrentlyN numProcs f xs =
  if numProcs < 1
    then
      throwWithCallStack $
        MkPooledException $
          "pooledMapconcurrentlyN: number of threads < 1: " <> show numProcs
    else pooledMapConcurrently' numProcs f xs
{-# INLINEABLE pooledMapConcurrentlyN #-}

-- | @unliftio@'s @pooledMapConcurrently'@.
--
-- @since 0.1
pooledMapConcurrently' ::
  forall m t a b.
  ( HasCallStack,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m,
    Traversable t
  ) =>
  -- | Max. number of threads. Should not be less than 1.
  Int ->
  (a -> m b) ->
  t a ->
  m (t b)
pooledMapConcurrently' numProcs f xs
  | numProcs < 1 =
      throwWithCallStack $
        MkPooledException "pooledMapConcurrently': empty IORef"
  | otherwise = do
      -- prepare one IORef per result...
      jobs :: t (a, IORef b) <-
        for xs (\x -> (x,) <$> newIORef (error "pooledMapConcurrently': empty IORef"))
      -- ...put all the inputs in a queue..
      jobsVar :: IORef [(a, IORef b)] <- newIORef (toList jobs)
      -- ...run `numProcs` threads in parallel, each
      -- of them consuming the queue and filling in
      -- the respective IORefs.
      pooledConcurrently numProcs jobsVar $ \(x, outRef) ->
        f x
          >>= atomicWriteIORef outRef -- Read all the IORefs
      for jobs (\(_, outputRef) -> readIORef outputRef)
{-# INLINEABLE pooledMapConcurrently' #-}

-- | @unliftio@'s @pooledMapConcurrentlyN_@.
--
-- @since 0.1
pooledMapConcurrentlyN_ ::
  forall m f a b.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadCallStack m,
    MonadIORef m
  ) =>
  Int ->
  (a -> m b) ->
  f a ->
  m ()
pooledMapConcurrentlyN_ numProcs f xs =
  if numProcs < 1
    then
      throwWithCallStack $
        MkPooledException $
          "pooledMapConcurrentlyN_: number of threads < 1: "
            <> show numProcs
    else pooledMapConcurrentlyN_' numProcs (\x -> f x $> ()) xs
{-# INLINEABLE pooledMapConcurrentlyN_ #-}

-- | @unliftio@'s @pooledMapConcurrentlyN_'@.
--
-- @since 0.1
pooledMapConcurrentlyN_' ::
  forall m f a.
  ( HasCallStack,
    Foldable f,
    MonadAsync m,
    MonadIORef m
  ) =>
  Int ->
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
  Int ->
  IORef [a] ->
  (a -> m b) ->
  m ()
pooledConcurrently numProcs jobsVar f = do
  replicateConcurrently_ numProcs $ do
    let loop = do
          mbJob :: Maybe a <- atomicModifyIORef' jobsVar $ \case
            [] -> ([], Nothing)
            var : vars -> (vars, Just var)
          case mbJob of
            Nothing -> return ()
            Just x -> do
              _ <- f x
              loop
     in loop
{-# INLINEABLE pooledConcurrently #-}
