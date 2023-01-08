-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.MonadHandleWriter
  ( -- * Class
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
  -- | @since 0.1
  openBinaryFile :: HasCallStack => Path -> IOMode -> m Handle

  -- | @since 0.1
  withBinaryFile :: HasCallStack => Path -> IOMode -> (Handle -> m a) -> m a

  -- | @since 0.1
  hClose :: HasCallStack => Handle -> m ()

  -- | @since 0.1
  hFlush :: HasCallStack => Handle -> m ()

  -- | @since 0.1
  hSetFileSize :: HasCallStack => Handle -> Integer -> m ()

  -- | @since 0.1
  hSetBuffering :: HasCallStack => Handle -> BufferMode -> m ()

  -- | @since 0.1
  hSeek :: HasCallStack => Handle -> SeekMode -> Integer -> m ()

  -- | @since 0.1
  hTell :: HasCallStack => Handle -> m Integer

  -- | @since 0.1
  hSetEcho :: HasCallStack => Handle -> Bool -> m ()

  -- | @since 0.1
  hPut :: HasCallStack => Handle -> ByteString -> m ()

  -- | @since 0.1
  hPutNonBlocking :: HasCallStack => Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile f = addCallStack . openBinaryFileIO f
  withBinaryFile f m = addCallStack . withBinaryFileIO f m
  hClose = addCallStack . IO.hClose
  hFlush = addCallStack . IO.hFlush
  hSetFileSize h = addCallStack . IO.hSetFileSize h
  hSetBuffering h = addCallStack . IO.hSetBuffering h
  hSeek h m = addCallStack . IO.hSeek h m
  hTell = addCallStack . IO.hTell
  hSetEcho h = addCallStack . IO.hSetEcho h
  hPut h = addCallStack . BS.hPut h
  hPutNonBlocking h = addCallStack . BS.hPutNonBlocking h

-- | @since 0.1
instance MonadHandleWriter m => MonadHandleWriter (ReaderT env m) where
  openBinaryFile p = lift . openBinaryFile p
  withBinaryFile p m f =
    ask >>= lift . \e -> withBinaryFile p m ((`runReaderT` e) . f)
  hClose = lift . hClose
  hFlush = lift . hFlush
  hSetFileSize h = lift . hSetFileSize h
  hSetBuffering h = lift . hSetBuffering h
  hSeek h m = lift . hSeek h m
  hTell = lift . hTell
  hSetEcho h = lift . hSetEcho h
  hPut h = lift . hPut h
  hPutNonBlocking h = lift . hPutNonBlocking h

-- | @since 0.1
hPutUtf8 :: (HasCallStack, MonadHandleWriter m) => Handle -> Text -> m ()
hPutUtf8 h = hPut h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8' ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m ByteString
hPutNonBlockingUtf8' h = hPutNonBlocking h . encodeUtf8

-- | @since 0.1
hPutNonBlockingUtf8 ::
  ( HasCallStack,
    MonadHandleWriter m
  ) =>
  Handle ->
  Text ->
  m (Either UnicodeException Text)
hPutNonBlockingUtf8 h = fmap decodeUtf8 . hPutNonBlocking h . encodeUtf8

-- | @since 0.1
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

-- | @since 0.1
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
