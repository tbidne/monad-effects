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

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effects.Exception (MonadThrow, exitFailure)
import Effects.FileSystem.Utils (OsPath)
import Effects.FileSystem.Utils qualified as FsUtils
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
  openBinaryFile :: OsPath -> IOMode -> m Handle

  -- | Lifted 'FsUtils.withBinaryFileIO'.
  --
  -- @since 0.1
  withBinaryFile :: OsPath -> IOMode -> (Handle -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: Handle -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: Handle -> m ()

  -- | Lifted 'IO.hSetFileSize'.
  --
  -- @since 0.1
  hSetFileSize :: Handle -> Integer -> m ()

  -- | Lifted 'IO.hSetBuffering'.
  --
  -- @since 0.1
  hSetBuffering :: Handle -> BufferMode -> m ()

  -- | Lifted 'IO.hSeek'.
  --
  -- @since 0.1
  hSeek :: Handle -> SeekMode -> Integer -> m ()

  -- | Lifted 'IO.hTell'.
  --
  -- @since 0.1
  hTell :: Handle -> m Integer

  -- | Lifted 'IO.hSetEcho'.
  --
  -- @since 0.1
  hSetEcho :: Handle -> Bool -> m ()

  -- | Lifted 'BS.hPut'.
  --
  -- @since 0.1
  hPut :: Handle -> ByteString -> m ()

  -- | Lifted 'BS.hPutNonBlocking'.
  --
  -- @since 0.1
  hPutNonBlocking :: Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile = FsUtils.openBinaryFileIO
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile = FsUtils.withBinaryFileIO
  {-# INLINEABLE withBinaryFile #-}
  hClose = IO.hClose
  {-# INLINEABLE hClose #-}
  hFlush = IO.hFlush
  {-# INLINEABLE hFlush #-}
  hTell = IO.hTell
  {-# INLINEABLE hTell #-}
  hSetFileSize = IO.hSetFileSize
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering = IO.hSetBuffering
  {-# INLINEABLE hSetBuffering #-}
  hSeek = IO.hSeek
  {-# INLINEABLE hSeek #-}
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
hPutUtf8 :: (MonadHandleWriter m) => Handle -> Text -> m ()
hPutUtf8 h = hPut h . FsUtils.encodeUtf8
{-# INLINEABLE hPutUtf8 #-}

-- | Writes UTF-8 text to handle, returning leftover bytes.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ByteString
hPutNonBlockingUtf8 h = hPutNonBlocking h . FsUtils.encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8 #-}

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 0.1
die :: (MonadHandleWriter m, MonadThrow m) => String -> m a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
