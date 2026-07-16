-- | Provides the MonadHandleReader effect.
--
-- @since 0.1
module Effects.FileSystem.HandleReader
  ( -- * Effect
    MonadHandleReader (..),
    Handle,
    HandleMode (HandleModeRead),
    CanRead,

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

    -- ** GetLine
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,

    -- ** GetContents
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,

    -- ** Get
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,

    -- ** GetSome
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,

    -- ** GetNonBlocking
    hGetNonBlockingUtf8,
    hGetNonBlockingUtf8Lenient,
    hGetNonBlockingUtf8ThrowM,

    -- * Reexports
    ByteString,
    OsPath,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Catch (MonadMask, MonadThrow, bracket, bracket_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effects.FileSystem.Handle.Internal
  ( CanRead,
    Handle (MkHandle),
    HandleMode (HandleModeRead),
    LockedHandle,
  )
import Effects.FileSystem.Handle.Internal qualified as Internal
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.IO.Handle.Lock qualified as Lock
import GHC.Stack (HasCallStack)
import System.IO (BufferMode, IOMode (ReadMode))
import System.IO qualified as IO

-- | Represents handle reader effects.
--
-- @since 0.1
class (Monad m) => MonadHandleReader m where
  -- | Opens a file for reading.
  --
  -- @since 0.1
  openBinaryFile :: (HasCallStack) => OsPath -> m (Handle HandleModeRead)

  -- | Opens a file for reading.
  --
  -- @since 0.1
  withBinaryFile :: (HasCallStack) => OsPath -> (Handle HandleModeRead -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: (CanRead p, HasCallStack) => Handle p -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: (CanRead p, HasCallStack) => Handle p -> m ()

  -- | Lifted 'IO.hIsEOF'.
  --
  -- @since 0.1
  hIsEOF :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hGetBuffering'.
  --
  -- @since 0.1
  hGetBuffering :: (CanRead p, HasCallStack) => Handle p -> m BufferMode

  -- | Lifted 'IO.hIsOpen'.
  --
  -- @since 0.1
  hIsOpen :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hIsClosed'.
  --
  -- @since 0.1
  hIsClosed :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hIsReadable'.
  --
  -- @since 0.1
  hIsReadable :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hIsWritable'.
  --
  -- @since 0.1
  hIsWritable :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hIsSeekable'.
  --
  -- @since 0.1
  hIsSeekable :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hIsTerminalDevice'.
  --
  -- @since 0.1
  hIsTerminalDevice :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hGetEcho'.
  --
  -- @since 0.1
  hGetEcho :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hWaitForInput'.
  --
  -- @since 0.1
  hWaitForInput :: (CanRead p, HasCallStack) => Handle p -> Int -> m Bool

  -- | Lifted 'IO.hReady'.
  --
  -- @since 0.1
  hReady :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Lifted 'IO.hGetChar'.
  --
  -- @since 0.1
  hGetChar :: (CanRead p, HasCallStack) => Handle p -> m Char

  -- | Lifted 'C8.hGetLine'.
  --
  -- @since 0.1
  hGetLine :: (CanRead p, HasCallStack) => Handle p -> m ByteString

  -- | Lifted 'C8.hGetContents'.
  --
  -- @since 0.1
  hGetContents :: (CanRead p, HasCallStack) => Handle p -> m ByteString

  -- | Lifted 'C8.hGet'.
  --
  -- @since 0.1
  hGet :: (CanRead p, HasCallStack) => Handle p -> Int -> m ByteString

  -- | Lifted 'C8.hGetSome'.
  --
  -- @since 0.1
  hGetSome :: (CanRead p, HasCallStack) => Handle p -> Int -> m ByteString

  -- | Lifted 'C8.hGetNonBlocking'.
  --
  -- @since 0.1
  hGetNonBlocking :: (CanRead p, HasCallStack) => Handle p -> Int -> m ByteString

  -- | Attempts to shared lock a file, blocking or throwing an exception
  -- upon failure.
  --
  -- @since 0.1
  hLockRaw :: (CanRead p, HasCallStack) => Handle p -> m ()

  -- | Attempts to shared lock a file.
  --
  -- @since 0.1
  hTryLockRaw :: (CanRead p, HasCallStack) => Handle p -> m Bool

  -- | Unlocks a locked file.
  --
  -- @since 0.1
  hUnlockRaw :: (CanRead p, HasCallStack) => Handle p -> m ()

-- | @since 0.1
instance MonadHandleReader IO where
  openBinaryFile p = MkHandle <$> FS.IO.openBinaryFileIO p ReadMode
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p onHandle =
    FS.IO.withBinaryFileIO p ReadMode (onHandle . MkHandle)
  {-# INLINEABLE withBinaryFile #-}
  hClose = IO.hClose . Internal.unHandle
  {-# INLINEABLE hClose #-}
  hFlush = IO.hFlush . Internal.unHandle
  {-# INLINEABLE hFlush #-}
  hIsEOF = IO.hIsEOF . Internal.unHandle
  {-# INLINEABLE hIsEOF #-}
  hGetBuffering = IO.hGetBuffering . Internal.unHandle
  {-# INLINEABLE hGetBuffering #-}
  hIsOpen = IO.hIsOpen . Internal.unHandle
  {-# INLINEABLE hIsOpen #-}
  hIsClosed = IO.hIsClosed . Internal.unHandle
  {-# INLINEABLE hIsClosed #-}
  hIsReadable = IO.hIsReadable . Internal.unHandle
  {-# INLINEABLE hIsReadable #-}
  hIsWritable = IO.hIsWritable . Internal.unHandle
  {-# INLINEABLE hIsWritable #-}
  hIsSeekable = IO.hIsSeekable . Internal.unHandle
  {-# INLINEABLE hIsSeekable #-}
  hIsTerminalDevice = IO.hIsTerminalDevice . Internal.unHandle
  {-# INLINEABLE hIsTerminalDevice #-}
  hGetEcho = IO.hGetEcho . Internal.unHandle
  {-# INLINEABLE hGetEcho #-}
  hWaitForInput = IO.hWaitForInput . Internal.unHandle
  {-# INLINEABLE hWaitForInput #-}
  hReady = IO.hReady . Internal.unHandle
  {-# INLINEABLE hReady #-}
  hGetChar = IO.hGetChar . Internal.unHandle
  {-# INLINEABLE hGetChar #-}
  hGetLine = C8.hGetLine . Internal.unHandle
  {-# INLINEABLE hGetLine #-}
  hGetContents = C8.hGetContents . Internal.unHandle
  {-# INLINEABLE hGetContents #-}
  hGet = C8.hGet . Internal.unHandle
  {-# INLINEABLE hGet #-}
  hGetSome = C8.hGetSome . Internal.unHandle
  {-# INLINEABLE hGetSome #-}
  hGetNonBlocking = C8.hGetNonBlocking . Internal.unHandle
  {-# INLINEABLE hGetNonBlocking #-}
  hLockRaw h = Lock.hLock (Internal.unHandle h) Lock.SharedLock
  {-# INLINEABLE hLockRaw #-}
  hTryLockRaw h = Lock.hTryLock (Internal.unHandle h) Lock.SharedLock
  {-# INLINEABLE hTryLockRaw #-}
  hUnlockRaw h = Lock.hUnlock (Internal.unHandle h)
  {-# INLINEABLE hUnlockRaw #-}

-- | @since 0.1
instance (MonadHandleReader m) => MonadHandleReader (ReaderT e m) where
  openBinaryFile = lift . openBinaryFile
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile p f = ask >>= lift . \e -> withBinaryFile p ((`runReaderT` e) . f)
  {-# INLINEABLE withBinaryFile #-}
  hClose = lift . hClose
  {-# INLINEABLE hClose #-}
  hFlush = lift . hFlush
  {-# INLINEABLE hFlush #-}
  hIsEOF = lift . hIsEOF
  {-# INLINEABLE hIsEOF #-}
  hGetBuffering = lift . hGetBuffering
  {-# INLINEABLE hGetBuffering #-}
  hIsOpen = lift . hIsOpen
  {-# INLINEABLE hIsOpen #-}
  hIsClosed = lift . hIsClosed
  {-# INLINEABLE hIsClosed #-}
  hIsReadable = lift . hIsReadable
  {-# INLINEABLE hIsReadable #-}
  hIsWritable = lift . hIsWritable
  {-# INLINEABLE hIsWritable #-}
  hIsSeekable = lift . hIsSeekable
  {-# INLINEABLE hIsSeekable #-}
  hIsTerminalDevice = lift . hIsTerminalDevice
  {-# INLINEABLE hIsTerminalDevice #-}
  hGetEcho = lift . hGetEcho
  {-# INLINEABLE hGetEcho #-}
  hWaitForInput h = lift . hWaitForInput h
  {-# INLINEABLE hWaitForInput #-}
  hReady = lift . hReady
  {-# INLINEABLE hReady #-}
  hGetChar = lift . hGetChar
  {-# INLINEABLE hGetChar #-}
  hGetLine = lift . hGetLine
  {-# INLINEABLE hGetLine #-}
  hGetContents = lift . hGetContents
  {-# INLINEABLE hGetContents #-}
  hGet h = lift . hGet h
  {-# INLINEABLE hGet #-}
  hGetSome h = lift . hGetSome h
  {-# INLINEABLE hGetSome #-}
  hGetNonBlocking h = lift . hGetNonBlocking h
  {-# INLINEABLE hGetNonBlocking #-}
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
-- main :: (MonadHandleReader m) => m ()
-- main = withBinaryFile path $ \\h -> do
--   hLockRaw handle
--   bs <- readBytes @HandleModeRead h
--   hUnlockRaw handle
--   print bs
--
-- readBytes :: (CanRead p, MonadHandleReader m) => Handle p -> m ByteString
-- readBytes handle = hGet handle 1024
-- @
--
-- In this example, we could remove all locking logic from @main@ and
-- everything would still compile. On the other hand:
--
-- @
-- main :: (MonadHandleReader m) => m ()
-- main = withBinaryFile path $ \\h -> withLockedFile h $ \\lh -> do
--   bs <- readBytes @HandleModeRead lh
--   print bs
--
-- readBytes :: (CanRead p, MonadHandleReader m) => LockedHandle p -> m ByteString
-- readBytes lockedHandle = liftLocked (\\h -\> hGet h 1024) lockedHandle
-- @
--
-- Removing @withLockedFile@ would cause a compilation error, since @readBytes@
-- requires a @LockedHandle@. The idea is to write most of the program's
-- logic in terms of @LockedHandle@, using @liftLocked@ to lift @Handle@
-- functions.

-- | Like 'hLockRaw', but returns a 'LockedHandle'.
--
-- @since 0.1
hLock ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  -- | Handle to lock.
  Handle p ->
  m (LockedHandle p)
hLock = Internal.liftLock hLockRaw

-- | Like 'hTryLockRaw', but returns a 'LockedHandle' if it succeeds.
--
-- @since 0.1
hTryLock ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  -- | Handle to lock.
  Handle p ->
  m (Maybe (LockedHandle p))
hTryLock = Internal.liftTryLock hTryLockRaw

-- | Like 'hUnlockRaw', but returns the original handle.
--
-- @since 0.1
hUnlock ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  -- | Handle to unlock.
  LockedHandle p ->
  m (Handle p)
hUnlock = Internal.liftUnlock hUnlockRaw

-- | Runs a computation with a shared locked file.
--
-- @since 0.1
withLockedFile ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
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
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
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
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
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
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
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

-- | 'hGetLine' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetLineUtf8 ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  m (Either UnicodeException Text)
hGetLineUtf8 = fmap FS.UTF8.decodeUtf8 . hGetLine
{-# INLINEABLE hGetLineUtf8 #-}

-- | 'hGetLine' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetLineUtf8Lenient ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  m Text
hGetLineUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetLine
{-# INLINEABLE hGetLineUtf8Lenient #-}

-- | 'hGetLine' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle p ->
  m Text
hGetLineUtf8ThrowM = hGetLine >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE hGetLineUtf8ThrowM #-}

-- | 'hGetContents' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  m (Either UnicodeException Text)
hGetContentsUtf8 = fmap FS.UTF8.decodeUtf8 . hGetContents
{-# INLINEABLE hGetContentsUtf8 #-}

-- | 'hGetContents' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  m Text
hGetContentsUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . hGetContents
{-# INLINEABLE hGetContentsUtf8Lenient #-}

-- | 'hGetContents' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle p ->
  m Text
hGetContentsUtf8ThrowM = hGetContents >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE hGetContentsUtf8ThrowM #-}

-- | 'hGet' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetUtf8 ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m (Either UnicodeException Text)
hGetUtf8 h = fmap FS.UTF8.decodeUtf8 . hGet h
{-# INLINEABLE hGetUtf8 #-}

-- | 'hGet' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGet h
{-# INLINEABLE hGetUtf8Lenient #-}

-- | 'hGet' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetUtf8ThrowM h = hGet h >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE hGetUtf8ThrowM #-}

-- | 'hGetSome' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m (Either UnicodeException Text)
hGetSomeUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetSome h
{-# INLINEABLE hGetSomeUtf8 #-}

-- | 'hGetSome' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetSomeUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetSome h
{-# INLINEABLE hGetSomeUtf8Lenient #-}

-- | 'hGetSome' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetSomeUtf8ThrowM h = hGetSome h >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE hGetSomeUtf8ThrowM #-}

-- | 'hGetNonBlocking' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap FS.UTF8.decodeUtf8 . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8 #-}

-- | 'hGetNonBlocking' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetNonBlockingUtf8Lenient h = fmap FS.UTF8.decodeUtf8Lenient . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8Lenient #-}

-- | 'hGetNonBlocking' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( CanRead p,
    HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle p ->
  Int ->
  m Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE hGetNonBlockingUtf8ThrowM #-}
