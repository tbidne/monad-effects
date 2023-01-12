-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.MonadHandleWriter
  ( -- * Effect
    MonadHandleWriter (..),
    Path,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8',
    hPutNonBlockingUtf8,
    hPutNonBlockingUtf8Lenient,
    hPutNonBlockingUtf8ThrowM,

    -- * Reexports
    BufferMode (..),
    ByteString,
    IOMode (..),
    Handle,
    SeekMode (..),
    Text,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Effects.FileSystem.MonadFileReader
  ( UnicodeException,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effects.FileSystem.MonadFileWriter (encodeUtf8)
import Effects.FileSystem.Path (Path, openBinaryFileIO, withBinaryFileIO)
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Stack (HasCallStack)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | Represents handle writer effects.
--
-- @since 0.1
class Monad m => MonadHandleWriter m where
  -- | Lifted 'IO.openBinaryFile'.
  --
  -- @since 0.1
  openBinaryFile :: HasCallStack => Path -> IOMode -> m Handle

  -- | Lifted 'IO.withBinaryFile'.
  --
  -- @since 0.1
  withBinaryFile :: HasCallStack => Path -> IOMode -> (Handle -> m a) -> m a

  -- | Lifted 'IO.hClose'.
  --
  -- @since 0.1
  hClose :: HasCallStack => Handle -> m ()

  -- | Lifted 'IO.hFlush'.
  --
  -- @since 0.1
  hFlush :: HasCallStack => Handle -> m ()

  -- | Lifted 'IO.hSetFileSize'.
  --
  -- @since 0.1
  hSetFileSize :: HasCallStack => Handle -> Integer -> m ()

  -- | Lifted 'IO.hSetBuffering'.
  --
  -- @since 0.1
  hSetBuffering :: HasCallStack => Handle -> BufferMode -> m ()

  -- | Lifted 'IO.hSeek'.
  --
  -- @since 0.1
  hSeek :: HasCallStack => Handle -> SeekMode -> Integer -> m ()

  -- | Lifted 'IO.hTell'.
  --
  -- @since 0.1
  hTell :: HasCallStack => Handle -> m Integer

  -- | Lifted 'IO.hSetEcho'.
  --
  -- @since 0.1
  hSetEcho :: HasCallStack => Handle -> Bool -> m ()

  -- | Lifted 'BS.hPut'.
  --
  -- @since 0.1
  hPut :: HasCallStack => Handle -> ByteString -> m ()

  -- | Lifted 'BS.hPutNonBlocking'.
  --
  -- @since 0.1
  hPutNonBlocking :: HasCallStack => Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile f = addCallStack . openBinaryFileIO f
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile f m = addCallStack . withBinaryFileIO f m
  {-# INLINEABLE withBinaryFile #-}
  hClose = addCallStack . IO.hClose
  {-# INLINEABLE hClose #-}
  hFlush = addCallStack . IO.hFlush
  {-# INLINEABLE hFlush #-}
  hSetFileSize h = addCallStack . IO.hSetFileSize h
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering h = addCallStack . IO.hSetBuffering h
  {-# INLINEABLE hSetBuffering #-}
  hSeek h m = addCallStack . IO.hSeek h m
  {-# INLINEABLE hSeek #-}
  hTell = addCallStack . IO.hTell
  {-# INLINEABLE hTell #-}
  hSetEcho h = addCallStack . IO.hSetEcho h
  {-# INLINEABLE hSetEcho #-}
  hPut h = addCallStack . BS.hPut h
  {-# INLINEABLE hPut #-}
  hPutNonBlocking h = addCallStack . BS.hPutNonBlocking h
  {-# INLINEABLE hPutNonBlocking #-}

-- | @since 0.1
instance MonadHandleWriter m => MonadHandleWriter (ReaderT env m) where
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
hPutUtf8 h = hPut h . encodeUtf8
{-# INLINEABLE hPutUtf8 #-}

-- | Writes UTF-8 text to handle, returning leftover bytes.
--
-- @since 0.1
hPutNonBlockingUtf8' ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ByteString
hPutNonBlockingUtf8' h = hPutNonBlocking h . encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8' #-}

-- | Writes UTF-8 text to handle. Any leftover bytes are returned, after
-- attempted UTF-8 decoding.
--
-- @since 0.1
hPutNonBlockingUtf8 ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m (Either UnicodeException Text)
hPutNonBlockingUtf8 h = fmap decodeUtf8 . hPutNonBlocking h . encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8 #-}

-- | Writes UTF-8 text to handle. Any leftover bytes are returned, after
-- lenient UTF-8 conversion.
--
-- @since 0.1
hPutNonBlockingUtf8Lenient ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m Text
hPutNonBlockingUtf8Lenient h =
  fmap decodeUtf8Lenient
    . hPutNonBlocking h
    . encodeUtf8
{-# INLINEABLE hPutNonBlockingUtf8Lenient #-}

-- | Writes UTF-8 text to handle. Any leftover bytes are returned, after
-- UTF-8 conversion. Throws 'UnicodeException' if there are any decode
-- errors.
--
-- @since 0.1
hPutNonBlockingUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m Text
hPutNonBlockingUtf8ThrowM h =
  (hPutNonBlocking h . encodeUtf8) >=> decodeUtf8ThrowM
{-# INLINEABLE hPutNonBlockingUtf8ThrowM #-}
