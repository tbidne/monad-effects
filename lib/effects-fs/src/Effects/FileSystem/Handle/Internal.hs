{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.FileSystem.Handle.Internal
  ( -- * Typed Handle
    Handle (..),
    unHandle,
    HandleMode (..),

    -- ** Type families
    CanRead,
    CanWrite,

    -- * Locks
    LockedHandle (..),
    unLockedHandle,
    liftLocked,

    -- ** Helpers
    liftLock,
    liftTryLock,
    liftUnlock,
    withLockedFile,
    withTryLockedFile,
  )
where

import Control.Monad.Catch (MonadMask, bracket)
import Data.Functor (($>), (<&>))
import Data.Kind (Constraint, Type)
import GHC.Records (HasField, getField)
import GHC.Stack.Types (HasCallStack)

-- TODO: Drop once GHC 9.2 dropped.
#if MIN_VERSION_base(4, 17, 0)
import GHC.TypeError qualified as TE
#else
import GHC.TypeLits qualified as TE
#endif
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import System.IO qualified as IO

-- | Possible handle modes.
--
-- @since 0.1
data HandleMode
  = -- | @since 0.1
    HandleModeRead
  | -- | @since 0.1
    HandleModeWrite
  | -- | @since 0.1
    HandleModeReadWrite

-- | Wrapper for file handles that includes an index for the mode.
--
-- @since 0.1
type Handle :: HandleMode -> Type
newtype Handle p = MkHandle IO.Handle

-- | @since 0.1
instance HasField "unHandle" (Handle p) IO.Handle where
  getField = unHandle

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ IO.Handle, b ~ IO.Handle) =>
  LabelOptic "unHandle" k (Handle p) (Handle p) a b
  where
  labelOptic = to unHandle
  {-# INLINE labelOptic #-}

-- | @since 0.1
unHandle :: Handle p -> IO.Handle
unHandle (MkHandle h) = h

{- ORMOLU_DISABLE -}

-- | @since 0.1
type CanRead :: HandleMode -> Constraint
type family CanRead hm where
  CanRead HandleModeRead = ()
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
  CanRead HandleModeWrite = TE.Unsatisfiable (TE.Text "HandleModeWrite does not have Read permission.")
#else
  CanRead HandleModeWrite = TE.TypeError (TE.Text "HandleModeWrite does not have Read permission.")
#endif
  CanRead HandleModeReadWrite = ()

-- | @since 0.1
type CanWrite :: HandleMode -> Constraint
type family CanWrite hm where
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
  CanWrite HandleModeRead = TE.Unsatisfiable (TE.Text "HandleModeRead does not have Read permission.")
#else
  CanWrite HandleModeRead = TE.TypeError (TE.Text "HandleModeRead does not have Read permission.")
#endif
  CanWrite HandleModeWrite = ()
  CanWrite HandleModeReadWrite = ()

{- ORMOLU_ENABLE -}

-- | File handle with lock.
--
-- @since 0.1
type LockedHandle :: HandleMode -> Type
newtype LockedHandle p = MkLockedHandle (Handle p)

-- | @since 0.1
instance HasField "unLockedHandle" (LockedHandle p) (Handle p) where
  getField = unLockedHandle

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Handle p, b ~ Handle p) =>
  LabelOptic "unLockedHandle" k (LockedHandle p) (LockedHandle p) a b
  where
  labelOptic = to unLockedHandle
  {-# INLINE labelOptic #-}

-- | @since 0.1
unLockedHandle :: LockedHandle p -> Handle p
unLockedHandle (MkLockedHandle h) = h

liftLock ::
  (Functor f) =>
  (Handle p -> f ()) ->
  Handle p ->
  f (LockedHandle p)
liftLock lockFn handle = lockFn handle $> MkLockedHandle handle

liftTryLock ::
  (Functor f) =>
  (Handle p -> f Bool) ->
  Handle p ->
  f (Maybe (LockedHandle p))
liftTryLock tryLockFn handle =
  tryLockFn handle <&> \locked ->
    if locked
      then Just (MkLockedHandle handle)
      else Nothing

liftUnlock ::
  (Functor f) =>
  (Handle p -> f ()) ->
  LockedHandle p ->
  f (Handle p)
liftUnlock unlockFn (MkLockedHandle handle) = unlockFn handle $> handle

-- | Runs a computation with a locked file.
withLockedFile ::
  ( HasCallStack,
    MonadMask m
  ) =>
  -- | Lock function.
  (Handle p -> m ()) ->
  -- | Unlock function.
  (Handle p -> m ()) ->
  -- | Handle to lock.
  Handle p ->
  -- | Callback with locked handle.
  (LockedHandle p -> m a) ->
  m a
withLockedFile lockFn unlockFn handle =
  bracket
    (liftLock lockFn handle)
    (liftUnlock unlockFn)
{-# INLINEABLE withLockedFile #-}

-- | Like 'withSharedLockedFile', except the lock attempt does not block.
--
-- @since 0.1
withTryLockedFile ::
  forall p m a.
  ( HasCallStack,
    MonadMask m
  ) =>
  -- | Lock function.
  (Handle p -> m Bool) ->
  -- | Unlock function.
  (Handle p -> m ()) ->
  -- | Handle to lock.
  Handle p ->
  -- | Handle callback.
  (LockedHandle p -> m a) ->
  m (Maybe a)
withTryLockedFile tryLockFn unlockFn handle onHandle =
  bracket
    (liftTryLock tryLockFn handle)
    (traverse (liftUnlock unlockFn))
    (traverse onHandle)
{-# INLINEABLE withTryLockedFile #-}

-- | Lifts a function on handle to one on locked handles.
--
-- @since 0.1
liftLocked ::
  forall p a.
  (HasCallStack) =>
  ((HasCallStack) => Handle p -> a) ->
  LockedHandle p ->
  a
liftLocked onHandle (MkLockedHandle h) = onHandle h
{-# INLINEABLE liftLocked #-}
