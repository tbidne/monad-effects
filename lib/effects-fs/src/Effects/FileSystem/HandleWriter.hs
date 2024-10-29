-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.HandleWriter
  ( -- * Effect
    MonadHandleWriter (..),
    OsPath,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8,

    -- * Misc
    die,

    -- * Reexports
    BufferMode (..),
    ByteString,
    IOMode (..),
    Handle,
    SeekMode (..),
    Text,
  )
where

import Control.Exception.Utils (exitFailure)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.Stack (HasCallStack)
import System.IO
  ( BufferMode (BlockBuffering, LineBuffering, NoBuffering),
    Handle,
    IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode),
    SeekMode (AbsoluteSeek, RelativeSeek, SeekFromEnd),
  )
import System.IO qualified as IO

-- | Represents handle writer effects.
--
-- @since 0.1
class (Monad m) => MonadHandleWriter m where
  -- | Lifted 'FsUtils.openBinaryFileIO'.
  --
  -- @since 0.1
  openBinaryFile :: (HasCallStack) => OsPath -> IOMode -> m Handle

  -- | Lifted 'FsUtils.withBinaryFileIO'.
  --
  -- @since 0.1
  withBinaryFile :: (HasCallStack) => OsPath -> IOMode -> (Handle -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: (HasCallStack) => Handle -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: (HasCallStack) => Handle -> m ()

  -- | Lifted 'IO.hSetFileSize'.
  --
  -- @since 0.1
  hSetFileSize :: (HasCallStack) => Handle -> Integer -> m ()

  -- | Lifted 'IO.hSetBuffering'.
  --
  -- @since 0.1
  hSetBuffering :: (HasCallStack) => Handle -> BufferMode -> m ()

  -- | Lifted 'IO.hSeek'.
  --
  -- @since 0.1
  hSeek :: (HasCallStack) => Handle -> SeekMode -> Integer -> m ()

  -- | Lifted 'IO.hTell'.
  --
  -- @since 0.1
  hTell :: (HasCallStack) => Handle -> m Integer

  -- | Lifted 'IO.hSetEcho'.
  --
  -- @since 0.1
  hSetEcho :: (HasCallStack) => Handle -> Bool -> m ()

  -- | Lifted 'BS.hPut'.
  --
  -- @since 0.1
  hPut :: (HasCallStack) => Handle -> ByteString -> m ()

  -- | Lifted 'BS.hPutNonBlocking'.
  --
  -- @since 0.1
  hPutNonBlocking :: (HasCallStack) => Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile = FS.IO.openBinaryFileIO
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile = FS.IO.withBinaryFileIO
  {-# INLINEABLE withBinaryFile #-}
  hClose = IO.hClose
  {-# INLINEABLE hClose #-}
  hFlush = IO.hFlush
  {-# INLINEABLE hFlush #-}
  hSetFileSize = IO.hSetFileSize
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering = IO.hSetBuffering
  {-# INLINEABLE hSetBuffering #-}
  hSeek = IO.hSeek
  {-# INLINEABLE hSeek #-}
  hTell = IO.hTell
  {-# INLINEABLE hTell #-}
  hSetEcho = IO.hSetEcho
  {-# INLINEABLE hSetEcho #-}
  hPut = BS.hPut
  {-# INLINEABLE hPut #-}
  hPutNonBlocking = BS.hPutNonBlocking
  {-# INLINEABLE hPutNonBlocking #-}

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

-- | Writes the UTF-8 text to the handle.
--
-- @since 0.1
hPutUtf8 :: (HasCallStack, MonadHandleWriter m) => Handle -> Text -> m ()
hPutUtf8 h = hPut h . FS.UTF8.encodeUtf8
{-# INLINEABLE hPutUtf8 #-}

-- | Writes UTF-8 text to handle, returning leftover bytes.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . FS.UTF8.encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8 #-}

-- | Write given error message to 'IO.stderr' and terminate with `exitFailure`.
--
-- @since 0.1
die :: (HasCallStack, MonadHandleWriter m, MonadThrow m) => String -> m a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
