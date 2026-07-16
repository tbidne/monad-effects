-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.HandleWriter
  ( -- * Effect
    MonadHandleWriter (..),
    Handle,
    HandleMode (HandleModeWrite),
    CanWrite,

    -- * Locking
    -- $locking
    LockedHandle,
    Internal.liftLocked,
    withLockedFile,
    withTryLockedFile,
    hLock,
    hTryLock,
    hUnlock,

    -- ** Raw
    withLockedFileRaw,
    withTryLockedFileRaw,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8,

    -- * Misc
    die,

    -- * Reexports
    BufferMode (..),
    ByteString,
    IOMode (..),
    OsPath,
    SeekMode (..),
    Text,
  )
where

import Control.Exception.Utils (exitFailure)
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, bracket_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effects.FileSystem.Handle qualified as Handle
import Effects.FileSystem.Handle.Internal
  ( CanWrite,
    Handle (MkHandle),
    HandleMode (HandleModeWrite),
    LockedHandle,
  )
import Effects.FileSystem.Handle.Internal qualified as Internal
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.IO.Handle.Lock qualified as Lock
import GHC.Stack (HasCallStack)
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )
import System.IO qualified as IO

-- | Represents handle writer effects.
--
-- @since 0.1
class (Monad m) => MonadHandleWriter m where
  -- | Opens a file for writing. If the boolean is true, opens in AppendMode.
  -- Otherwise opens in WriteMode.
  --
  -- @since 0.1
  openBinaryFile :: (HasCallStack) => OsPath -> Bool -> m (Handle HandleModeWrite)

  -- | Opens a file for writing. If the boolean is true, opens in AppendMode.
  -- Otherwise opens in WriteMode.
  --
  -- @since 0.1
  withBinaryFile :: (HasCallStack) => OsPath -> Bool -> (Handle HandleModeWrite -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: (CanWrite p, HasCallStack) => Handle p -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: (CanWrite p, HasCallStack) => Handle p -> m ()

  -- | Lifted 'IO.hSetFileSize'.
  --
  -- @since 0.1
  hSetFileSize :: (CanWrite p, HasCallStack) => Handle p -> Integer -> m ()

  -- | Lifted 'IO.hSetBuffering'.
  --
  -- @since 0.1
  hSetBuffering :: (CanWrite p, HasCallStack) => Handle p -> BufferMode -> m ()

  -- | Lifted 'IO.hSeek'.
  --
  -- @since 0.1
  hSeek :: (CanWrite p, HasCallStack) => Handle p -> SeekMode -> Integer -> m ()

  -- | Lifted 'IO.hTell'.
  --
  -- @since 0.1
  hTell :: (CanWrite p, HasCallStack) => Handle p -> m Integer

  -- | Lifted 'IO.hSetEcho'.
  --
  -- @since 0.1
  hSetEcho :: (CanWrite p, HasCallStack) => Handle p -> Bool -> m ()

  -- | Lifted 'BS.hPut'.
  --
  -- @since 0.1
  hPut :: (CanWrite p, HasCallStack) => Handle p -> ByteString -> m ()

  -- | Lifted 'BS.hPutNonBlocking'.
  --
  -- @since 0.1
  hPutNonBlocking :: (CanWrite p, HasCallStack) => Handle p -> ByteString -> m ByteString

  -- | Attempts to exclusively lock a file, blocking or throwing an exception
  -- upon failure.
  --
  -- @since 0.1
  hLockRaw :: (CanWrite p, HasCallStack) => Handle p -> m ()

  -- | Attempts to exclusively lock a file.
  --
  -- @since 0.1
  hTryLockRaw :: (CanWrite p, HasCallStack) => Handle p -> m Bool

  -- | Unlocks a locked file.
  --
  -- @since 0.1
  hUnlockRaw :: (CanWrite p, HasCallStack) => Handle p -> m ()

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile p = fmap MkHandle . FS.IO.openBinaryFileIO p . appendToMode
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p b onHandle =
    FS.IO.withBinaryFileIO p (appendToMode b) (onHandle . MkHandle)
  {-# INLINEABLE withBinaryFile #-}
  hClose = IO.hClose . Internal.unHandle
  {-# INLINEABLE hClose #-}
  hFlush = IO.hFlush . Internal.unHandle
  {-# INLINEABLE hFlush #-}
  hSetFileSize = IO.hSetFileSize . Internal.unHandle
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering = IO.hSetBuffering . Internal.unHandle
  {-# INLINEABLE hSetBuffering #-}
  hSeek = IO.hSeek . Internal.unHandle
  {-# INLINEABLE hSeek #-}
  hTell = IO.hTell . Internal.unHandle
  {-# INLINEABLE hTell #-}
  hSetEcho = IO.hSetEcho . Internal.unHandle
  {-# INLINEABLE hSetEcho #-}
  hPut = BS.hPut . Internal.unHandle
  {-# INLINEABLE hPut #-}
  hPutNonBlocking = BS.hPutNonBlocking . Internal.unHandle
  {-# INLINEABLE hPutNonBlocking #-}
  hLockRaw h = Lock.hLock (Internal.unHandle h) Lock.ExclusiveLock
  {-# INLINEABLE hLockRaw #-}
  hTryLockRaw h = Lock.hTryLock (Internal.unHandle h) Lock.ExclusiveLock
  {-# INLINEABLE hTryLockRaw #-}
  hUnlockRaw h = Lock.hUnlock (Internal.unHandle h)
  {-# INLINEABLE hUnlockRaw #-}

-- | @since 0.1
instance (MonadHandleWriter m) => MonadHandleWriter (ReaderT env m) where
  openBinaryFile p = lift . openBinaryFile p
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p m f =
    ask >>= lift . \e -> withBinaryFile p m ((`runReaderT` e) . f)
  {-# INLINEABLE withBinaryFile #-}
  hClose = lift . hClose
  {-# INLINEABLE hClose #-}
  hFlush = lift . hFlush
  {-# INLINEABLE hFlush #-}
  hSetFileSize h = lift . hSetFileSize h
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering h = lift . hSetBuffering h
  {-# INLINEABLE hSetBuffering #-}
  hSeek h m = lift . hSeek h m
  {-# INLINEABLE hSeek #-}
  hTell = lift . hTell
  {-# INLINEABLE hTell #-}
  hSetEcho h = lift . hSetEcho h
  {-# INLINEABLE hSetEcho #-}
  hPut h = lift . hPut h
  {-# INLINEABLE hPut #-}
  hPutNonBlocking h = lift . hPutNonBlocking h
  {-# INLINEABLE hPutNonBlocking #-}
  hLockRaw = lift . hLockRaw
  {-# INLINEABLE hLockRaw #-}
  hTryLockRaw = lift . hTryLockRaw
  {-# INLINEABLE hTryLockRaw #-}
  hUnlockRaw = lift . hUnlockRaw
  {-# INLINEABLE hUnlockRaw #-}

-- $locking
--
-- These functions bring some type-safety to file locking. Consider:
--
-- @
-- main :: (MonadHandleWriter m) => m ()
-- main = withBinaryFile path False $ \\h -> do
--   hLockRaw handle
--   bs <- writeBytes @HandleModeWrite h
--   hUnlockRaw handle
--   print bs
--
-- writeBytes :: (CanWrite p, MonadHandleWriter m) => Handle p -> m ()
-- writeBytes handle = hPut handle "some bytes"
-- @
--
-- In this example, we could remove all locking logic from @main@ and
-- everything would still compile. On the other hand:
--
-- @
-- main :: (MonadHandleWriter m) => m ()
-- main = withBinaryFile path False $ \\h -> withLockedFile h $ \\lh -> do
--   bs <- writeBytes @HandleModeWrite lh
--   print bs
--
-- writeBytes :: (CanWrite p, MonadHandleWriter m) => LockedHandle p -> m ByteString
-- writeBytes lockedHandle = liftLocked (\\h -\> hPut h "some bytes") lockedHandle
-- @
--
-- Removing @withLockedFile@ would cause a compilation error, since @writeBytes@
-- requires a @LockedHandle@. The idea is to write most of the program's
-- logic in terms of @LockedHandle@, using @liftLocked@ to lift @Handle@
-- functions.

-- | Like 'hLockRaw', but returns a 'LockedHandle'.
--
-- @since 0.1
hLock ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m
  ) =>
  -- | Handle to lock.
  Handle p ->
  m (LockedHandle p)
hLock = Internal.liftLock hLockRaw

-- | Like 'hTryLockRaw', but returns a 'LockedHandle' if it succeeds.
--
-- @since 0.1
hTryLock ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m
  ) =>
  -- | Handle to lock.
  Handle p ->
  m (Maybe (LockedHandle p))
hTryLock = Internal.liftTryLock hTryLockRaw

-- | Like 'hUnlockRaw', but returns the original handle.
--
-- @since 0.1
hUnlock ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m
  ) =>
  -- | Handle to unlock.
  LockedHandle p ->
  m (Handle p)
hUnlock = Internal.liftUnlock hUnlockRaw

-- | Runs a computation with an exclusively locked file.
--
-- @since 0.1
withLockedFile ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m,
    MonadMask m
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Callback with locked handle.
  (LockedHandle p -> m a) ->
  m a
withLockedFile = Internal.withLockedFile hLockRaw hUnlockRaw
{-# INLINEABLE withLockedFile #-}

-- | Like 'withSharedLockedFile', except the lock attempt does not block.
--
-- @since 0.1
withTryLockedFile ::
  forall p m a.
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m,
    MonadMask m
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Handle callback.
  (LockedHandle p -> m a) ->
  m (Maybe a)
withTryLockedFile = Internal.withTryLockedFile hTryLockRaw hUnlockRaw
{-# INLINEABLE withTryLockedFile #-}

-- | 'withLockedFile' without 'LockedHandle'.
--
-- @since 0.1
withLockedFileRaw ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m,
    MonadMask m
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Callback with locked handle.
  m a ->
  m a
withLockedFileRaw h = bracket_ (hLockRaw h) (hUnlockRaw h)
{-# INLINEABLE withLockedFileRaw #-}

-- | 'withTryLockedFileRaw' without 'LockedHandle'.
--
-- @since 0.1
withTryLockedFileRaw ::
  forall p m a.
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m,
    MonadMask m
  ) =>
  -- | Handle to lock.
  Handle p ->
  -- | Handle callback.
  m a ->
  m (Maybe a)
withTryLockedFileRaw h m =
  bracket
    ( do
        locked <- hTryLockRaw h
        pure $
          if locked
            then Just ()
            else Nothing
    )
    (traverse (const (hUnlockRaw h)))
    (traverse (const m))
{-# INLINEABLE withTryLockedFileRaw #-}

appendToMode :: Bool -> IOMode
appendToMode True = AppendMode
appendToMode False = WriteMode

-- | Writes the UTF-8 text to the handle.
--
-- @since 0.1
hPutUtf8 ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle p ->
  Text ->
  m ()
hPutUtf8 h = hPut h . FS.UTF8.encodeUtf8
{-# INLINEABLE hPutUtf8 #-}

-- | Writes UTF-8 text to handle, returning leftover bytes.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( CanWrite p,
    HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle p ->
  Text ->
  m ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . FS.UTF8.encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8 #-}

-- | Write given error message to 'IO.stderr' and terminate with `exitFailure`.
--
-- @since 0.1
die :: (HasCallStack, MonadHandleWriter m, MonadThrow m) => String -> m a
die err = hPut Handle.stderr err' *> exitFailure
  where
    err' = Char8.pack err
