-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.HandleWriter
  ( -- * Effect
    MonadHandleWriter (..),
    Path,

    -- * UTF-8 Utils
    hPutUtf8,
    hPutNonBlockingUtf8',
    hPutNonBlockingUtf8,
    hPutNonBlockingUtf8Lenient,
    hPutNonBlockingUtf8ThrowM,

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

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Effects.Exception (MonadThrow, addCS, exitFailure)
import Effects.FileSystem.FileReader
  ( UnicodeException,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effects.FileSystem.FileWriter (encodeUtf8)
import Effects.FileSystem.Path (Path, openBinaryFileIO, withBinaryFileIO)
import GHC.Stack (HasCallStack)
import System.IO (BufferMode (..), Handle, IOMode (..), SeekMode (..))
import System.IO qualified as IO

-- | Represents handle writer effects.
--
-- @since 0.1
class (Monad m) => MonadHandleWriter m where
  -- | Lifted 'IO.openBinaryFile'.
  --
  -- @since 0.1
  openBinaryFile :: (HasCallStack) => Path -> IOMode -> m Handle

  -- | Lifted 'IO.withBinaryFile'.
  --
  -- @since 0.1
  withBinaryFile :: (HasCallStack) => Path -> IOMode -> (Handle -> m a) -> m a

  -- | Computation 'hClose' @hdl@ makes handle @hdl@ closed. Before the
  -- computation finishes, if @hdl@ is writable its buffer is flushed as
  -- for 'hFlush'.
  -- Performing 'hClose' on a handle that has already been closed has no effect;
  -- doing so is not an error. All other operations on a closed handle will fail.
  -- If 'hClose' fails for any reason, any further operations (apart from
  -- 'hClose') on the handle will still fail as if @hdl@ had been successfully
  -- closed.
  --
  -- 'hClose' is an /interruptible operation/ in the sense described in
  -- "Control.Exception". If 'hClose' is interrupted by an asynchronous
  -- exception in the process of flushing its buffers, then the I/O device
  -- (e.g., file) will be closed anyway.
  --
  -- @since 0.1
  hClose :: (HasCallStack) => Handle -> m ()

  -- | The action 'hFlush' @hdl@ causes any items buffered for output
  -- in handle @hdl@ to be sent immediately to the operating system.
  --
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isFullError' if the device is full;
  --
  --  * 'System.IO.Error.isPermissionError' if a system resource limit would be
  --    exceeded. It is unspecified whether the characters in the buffer are
  --    discarded or retained under these circumstances.
  --
  -- @since 0.1
  hFlush :: (HasCallStack) => Handle -> m ()

  -- | 'hSetFileSize' @hdl@ @size@ truncates the physical file with handle
  -- @hdl@ to @size@ bytes.
  --
  -- @since 0.1
  hSetFileSize :: (HasCallStack) => Handle -> Integer -> m ()

  -- | Computation 'hSetBuffering' @hdl mode@ sets the mode of buffering for
  -- handle @hdl@ on subsequent reads and writes.
  --
  -- If the buffer mode is changed from 'BlockBuffering' or
  -- 'LineBuffering' to 'NoBuffering', then
  --
  --  * if @hdl@ is writable, the buffer is flushed as for 'hFlush';
  --
  --  * if @hdl@ is not writable, the contents of the buffer is discarded.
  --
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isPermissionError' if the handle has already been used
  --    for reading or writing and the implementation does not allow the
  --    buffering mode to be changed.
  --
  -- @since 0.1
  hSetBuffering :: (HasCallStack) => Handle -> BufferMode -> m ()

  -- | Computation 'hSeek' @hdl mode i@ sets the position of handle
  -- @hdl@ depending on @mode@.
  -- The offset @i@ is given in terms of 8-bit bytes.
  --
  -- If @hdl@ is block- or line-buffered, then seeking to a position which is not
  -- in the current buffer will first cause any items in the output buffer to be
  -- written to the device, and then cause the input buffer to be discarded.
  -- Some handles may not be seekable (see 'hIsSeekable'), or only support a
  -- subset of the possible positioning operations (for instance, it may only
  -- be possible to seek to the end of a tape, or to a positive offset from
  -- the beginning or current position).
  -- It is not possible to set a negative I\/O position, or for
  -- a physical file, an I\/O position beyond the current end-of-file.
  --
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isIllegalOperationError' if the Handle is not seekable,
  --    or does not support the requested seek mode.
  --
  --  * 'System.IO.Error.isPermissionError' if a system resource limit would be
  --    exceeded.
  --
  -- @since 0.1
  hSeek :: (HasCallStack) => Handle -> SeekMode -> Integer -> m ()

  -- | Computation 'hTell' @hdl@ returns the current position of the
  -- handle @hdl@, as the number of bytes from the beginning of
  -- the file. The value returned may be subsequently passed to
  -- 'hSeek' to reposition the handle to the current position.
  --
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isIllegalOperationError' if the Handle is not seekable.
  --
  -- @since 0.1
  hTell :: (HasCallStack) => Handle -> m Integer

  -- | Set the echoing status of a handle connected to a terminal.
  --
  -- @since 0.1
  hSetEcho :: (HasCallStack) => Handle -> Bool -> m ()

  -- | Outputs a 'ByteString' to the specified 'Handle'.
  --
  -- @since 0.1
  hPut :: (HasCallStack) => Handle -> ByteString -> m ()

  -- | Similar to 'hPut' except that it will never block. Instead it returns
  -- any tail that did not get written. This tail may be 'empty' in the case that
  -- the whole string was written, or the whole original string if nothing was
  -- written. Partial writes are also possible.
  --
  -- Note: on Windows and with Haskell implementation other than GHC, this
  -- function does not work correctly; it behaves identically to 'hPut'.
  --
  -- @since 0.1
  hPutNonBlocking :: (HasCallStack) => Handle -> ByteString -> m ByteString

-- | @since 0.1
instance MonadHandleWriter IO where
  openBinaryFile f = addCS . openBinaryFileIO f
  {-# INLINEABLE openBinaryFile #-}
  withBinaryFile f m = addCS . withBinaryFileIO f m
  {-# INLINEABLE withBinaryFile #-}
  hClose = addCS . IO.hClose
  {-# INLINEABLE hClose #-}
  hFlush = addCS . IO.hFlush
  {-# INLINEABLE hFlush #-}
  hSetFileSize h = addCS . IO.hSetFileSize h
  {-# INLINEABLE hSetFileSize #-}
  hSetBuffering h = addCS . IO.hSetBuffering h
  {-# INLINEABLE hSetBuffering #-}
  hSeek h m = addCS . IO.hSeek h m
  {-# INLINEABLE hSeek #-}
  hTell = addCS . IO.hTell
  {-# INLINEABLE hTell #-}
  hSetEcho h = addCS . IO.hSetEcho h
  {-# INLINEABLE hSetEcho #-}
  hPut h = addCS . BS.hPut h
  {-# INLINEABLE hPut #-}
  hPutNonBlocking h = addCS . BS.hPutNonBlocking h
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
    MonadHandleWriter m,
    MonadThrow m
  ) =>
  Handle ->
  Text ->
  m Text
hPutNonBlockingUtf8ThrowM h =
  (hPutNonBlocking h . encodeUtf8) >=> decodeUtf8ThrowM
{-# INLINEABLE hPutNonBlockingUtf8ThrowM #-}

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since 0.1
die :: (HasCallStack, MonadHandleWriter m, MonadThrow m) => String -> m a
die err = hPut IO.stderr err' *> exitFailure
  where
    err' = Char8.pack err
