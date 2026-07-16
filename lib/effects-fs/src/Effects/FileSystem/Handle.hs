-- | Typed file handles.
--
-- @since 0.1
module Effects.FileSystem.Handle
  ( -- * Typed handle
    Handle,
    unHandle,
    unsafeHandle,
    HandleMode (..),

    -- ** IO handles
    stdin,
    stdout,
    stderr,

    -- ** Type families
    CanRead,
    CanWrite,

    -- ** Aliases
    HandleIO,
    HandleM,
    HandleR,
    HandleW,
    HandleRW,

    -- * Locks
    LockedHandle,
    Internal.unLockedHandle,
    unsafeLockedHandle,
    Internal.liftLocked,

    -- ** Aliases
    LockedHandleR,
    LockedHandleW,
    LockedHandleRW,
  )
where

import Effects.FileSystem.Handle.Internal
  ( CanRead,
    CanWrite,
    Handle (MkHandle),
    HandleMode (HandleModeRead, HandleModeReadWrite, HandleModeWrite),
    LockedHandle (MkLockedHandle),
    unHandle,
  )
import Effects.FileSystem.Handle.Internal qualified as Internal
import System.IO qualified as IO

-- | Alias for base's System.IO.Handle.
--
-- @since 0.1
type HandleIO = IO.Handle

-- | Alias for typed Handle.
--
-- @since 0.1
type HandleM p = Handle p

-- | Readable handle.
--
-- @since 0.1
type HandleR = Handle HandleModeRead

-- | Writable handle.
--
-- @since 0.1
type HandleW = Handle HandleModeWrite

-- | Readable and writable handle.
--
-- @since 0.1
type HandleRW = Handle HandleModeReadWrite

-- | Makes a handle with any permissions. Useful for pseudo handles.
--
-- @since 0.1
unsafeHandle :: IO.Handle -> Handle p
unsafeHandle = MkHandle

-- | Makes a LockedHandle without performing any locking.
--
-- @since 0.1
unsafeLockedHandle :: Handle p -> LockedHandle p
unsafeLockedHandle = MkLockedHandle

-- | @since 0.1
stdin :: Handle HandleModeReadWrite
stdin = MkHandle IO.stdin

-- | @since 0.1
stdout :: Handle HandleModeReadWrite
stdout = MkHandle IO.stdout

-- | @since 0.1
stderr :: Handle HandleModeReadWrite
stderr = MkHandle IO.stderr

-- | Readable handle.
--
-- @since 0.1
type LockedHandleR = LockedHandle HandleModeRead

-- | Writable handle.
--
-- @since 0.1
type LockedHandleW = LockedHandle HandleModeWrite

-- | Readable and writable handle.
--
-- @since 0.1
type LockedHandleRW = LockedHandle HandleModeReadWrite
