-- | Provides the MonadHandleReader effect.
--
-- @since 0.1
module Effects.FileSystem.HandleReader
  ( -- * Effect
    MonadHandleReader (..),

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
    Handle,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effects.Exception (MonadThrow, addCS)
import Effects.FileSystem.FileReader
  ( decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import GHC.Stack (HasCallStack)
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Represents handle reader effects.
--
-- @since 0.1
class (Monad m) => MonadHandleReader m where
  -- | For a readable handle @hdl@, 'hIsEOF' @hdl@ returns
  -- 'True' if no further input can be taken from @hdl@ or for a
  -- physical file, if the current I\/O position is equal to the length of
  -- the file. Otherwise, it returns 'False'.
  --
  -- NOTE: 'hIsEOF' may block, because it has to attempt to read from
  -- the stream to determine whether there is any more data to be read.
  --
  -- @since 0.1
  hIsEOF :: (HasCallStack) => Handle -> m Bool

  -- | Computation 'hGetBuffering' @hdl@ returns the current buffering mode
  -- for @hdl@.
  --
  -- @since 0.1
  hGetBuffering :: (HasCallStack) => Handle -> m BufferMode

  -- | Lifted 'IO.hIsOpen'.
  --
  -- @since 0.1
  hIsOpen :: (HasCallStack) => Handle -> m Bool

  -- | Lifted 'IO.hIsClosed'.
  --
  -- @since 0.1
  hIsClosed :: (HasCallStack) => Handle -> m Bool

  -- | Lifted 'IO.hIsReadable'.
  --
  -- @since 0.1
  hIsReadable :: (HasCallStack) => Handle -> m Bool

  -- | Lifted 'IO.hIsWritable'.
  --
  -- @since 0.1
  hIsWritable :: (HasCallStack) => Handle -> m Bool

  -- | Lifted 'IO.hIsSeekable'.
  --
  -- @since 0.1
  hIsSeekable :: (HasCallStack) => Handle -> m Bool

  -- | Is the handle connected to a terminal?
  --
  -- On Windows the result of 'hIsTerminalDevide' might be misleading,
  -- because non-native terminals, such as MinTTY used in MSYS and Cygwin environments,
  -- are implemented via redirection.
  -- Use @System.Win32.Types.withHandleToHANDLE System.Win32.MinTTY.isMinTTYHandle@
  -- to recognise it. Also consider @ansi-terminal@ package for crossplatform terminal
  -- support.
  --
  -- @since 0.1
  hIsTerminalDevice :: (HasCallStack) => Handle -> m Bool

  -- | Get the echoing status of a handle connected to a terminal.
  --
  -- @since 0.1
  hGetEcho :: (HasCallStack) => Handle -> m Bool

  -- | Computation 'hWaitForInput' @hdl t@
  -- waits until input is available on handle @hdl@.
  -- It returns 'True' as soon as input is available on @hdl@,
  -- or 'False' if no input is available within @t@ milliseconds. Note that
  -- 'hWaitForInput' waits until one or more full /characters/ are available,
  -- which means that it needs to do decoding, and hence may fail
  -- with a decoding error.
  --
  -- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
  --
  -- This operation may fail with:
  --
  --  * 'isEOFError' if the end of file has been reached.
  --
  --  * a decoding error, if the input begins with an invalid byte sequence
  --    in this Handle's encoding.
  --
  -- NOTE for GHC users: unless you use the @-threaded@ flag,
  -- @hWaitForInput hdl t@ where @t >= 0@ will block all other Haskell
  -- threads for the duration of the call. It behaves like a
  -- @safe@ foreign call in this respect.
  --
  -- @since 0.1
  hWaitForInput :: (HasCallStack) => Handle -> Int -> m Bool

  -- | Computation 'hReady' @hdl@ indicates whether at least one item is
  -- available for input from handle @hdl@.
  --
  -- This operation may fail with:
  --
  --  * 'System.IO.Error.isEOFError' if the end of file has been reached.
  --
  -- @since 0.1
  hReady :: (HasCallStack) => Handle -> m Bool

  -- | Computation 'hGetChar' @hdl@ reads a character from the file or
  -- channel managed by @hdl@, blocking until a character is available.
  --
  -- This operation may fail with:
  --
  --  * 'isEOFError' if the end of file has been reached.
  --
  -- @since 0.1
  hGetChar :: (HasCallStack) => Handle -> m Char

  -- | Computation 'hGetLine' @hdl@ reads a line from the file or
  -- channel managed by @hdl@.
  --
  -- This operation may fail with:
  --
  --  * 'isEOFError' if the end of file is encountered when reading
  --    the /first/ character of the line.
  --
  -- If 'hGetLine' encounters end-of-file at any other point while reading
  -- in a line, it is treated as a line terminator and the (partial)
  -- line is returned.
  --
  -- @since 0.1
  hGetLine :: (HasCallStack) => Handle -> m ByteString

  -- | Read a handle's entire contents strictly into a 'ByteString'.
  --
  -- This function reads chunks at a time, increasing the chunk size on each
  -- read. The final string is then reallocated to the appropriate size. For
  -- files > half of available memory, this may lead to memory exhaustion.
  -- Consider using 'readFile' in this case.
  --
  -- The Handle is closed once the contents have been read,
  -- or if an exception is thrown.
  --
  -- @since 0.1
  hGetContents :: (HasCallStack) => Handle -> m ByteString

  -- | Read a 'ByteString' directly from the specified 'Handle'. This
  -- is far more efficient than reading the characters into a 'String'
  -- and then using 'pack'. First argument is the Handle to read from,
  -- and the second is the number of bytes to read. It returns the bytes
  -- read, up to n, or 'empty' if EOF has been reached.
  --
  -- 'hGet' is implemented in terms of 'hGetBuf'.
  --
  -- If the handle is a pipe or socket, and the writing end
  -- is closed, 'hGet' will behave as if EOF was reached.
  --
  -- @since 0.1
  hGet :: (HasCallStack) => Handle -> Int -> m ByteString

  -- | Like 'hGet', except that a shorter 'ByteString' may be returned
  -- if there are not enough bytes immediately available to satisfy the
  -- whole request. 'hGetSome' only blocks if there is no data
  -- available, and EOF has not yet been reached.
  --
  -- @since 0.1
  hGetSome :: (HasCallStack) => Handle -> Int -> m ByteString

  -- | hGetNonBlocking is similar to 'hGet', except that it will never block
  -- waiting for data to become available, instead it returns only whatever data
  -- is available. If there is no data available to be read, 'hGetNonBlocking'
  -- returns 'empty'.
  --
  -- Note: on Windows and with Haskell implementation other than GHC, this
  -- function does not work correctly; it behaves identically to 'hGet'.
  --
  -- @since 0.1
  hGetNonBlocking :: (HasCallStack) => Handle -> Int -> m ByteString

-- | @since 0.1
instance MonadHandleReader IO where
  hIsEOF = addCS . IO.hIsEOF
  {-# INLINEABLE hIsEOF #-}
  hGetBuffering = addCS . IO.hGetBuffering
  {-# INLINEABLE hGetBuffering #-}
  hIsOpen = addCS . IO.hIsOpen
  {-# INLINEABLE hIsOpen #-}
  hIsClosed = addCS . IO.hIsClosed
  {-# INLINEABLE hIsClosed #-}
  hIsReadable = addCS . IO.hIsReadable
  {-# INLINEABLE hIsReadable #-}
  hIsWritable = addCS . IO.hIsWritable
  {-# INLINEABLE hIsWritable #-}
  hIsSeekable = addCS . IO.hIsSeekable
  {-# INLINEABLE hIsSeekable #-}
  hIsTerminalDevice = addCS . IO.hIsTerminalDevice
  {-# INLINEABLE hIsTerminalDevice #-}
  hGetEcho = addCS . IO.hGetEcho
  {-# INLINEABLE hGetEcho #-}
  hWaitForInput h = addCS . IO.hWaitForInput h
  {-# INLINEABLE hWaitForInput #-}
  hReady = addCS . IO.hReady
  {-# INLINEABLE hReady #-}
  hGetChar = addCS . IO.hGetChar
  {-# INLINEABLE hGetChar #-}
  hGetLine = addCS . BS.hGetLine
  {-# INLINEABLE hGetLine #-}
  hGetContents = addCS . BS.hGetContents
  {-# INLINEABLE hGetContents #-}
  hGet h = addCS . BS.hGet h
  {-# INLINEABLE hGet #-}
  hGetSome h = addCS . BS.hGetSome h
  {-# INLINEABLE hGetSome #-}
  hGetNonBlocking h = addCS . BS.hGetNonBlocking h
  {-# INLINEABLE hGetNonBlocking #-}

-- | @since 0.1
instance (MonadHandleReader m) => MonadHandleReader (ReaderT e m) where
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

-- | 'hGetLine' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetLineUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetLineUtf8 = fmap decodeUtf8 . hGetLine
{-# INLINEABLE hGetLineUtf8 #-}

-- | 'hGetLine' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetLineUtf8Lenient :: (HasCallStack, MonadHandleReader m) => Handle -> m Text
hGetLineUtf8Lenient = fmap decodeUtf8Lenient . hGetLine
{-# INLINEABLE hGetLineUtf8Lenient #-}

-- | 'hGetLine' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetLineUtf8ThrowM ::
  ( HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  m Text
hGetLineUtf8ThrowM = hGetLine >=> decodeUtf8ThrowM
{-# INLINEABLE hGetLineUtf8ThrowM #-}

-- | 'hGetContents' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetContentsUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetContentsUtf8 = fmap decodeUtf8 . hGetContents
{-# INLINEABLE hGetContentsUtf8 #-}

-- | 'hGetContents' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetContentsUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8Lenient = fmap decodeUtf8Lenient . hGetContents
{-# INLINEABLE hGetContentsUtf8Lenient #-}

-- | 'hGetContents' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetContentsUtf8ThrowM ::
  ( HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8ThrowM = hGetContents >=> decodeUtf8ThrowM
{-# INLINEABLE hGetContentsUtf8ThrowM #-}

-- | 'hGet' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetUtf8 h = fmap decodeUtf8 . hGet h
{-# INLINEABLE hGetUtf8 #-}

-- | 'hGet' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8Lenient h = fmap decodeUtf8Lenient . hGet h
{-# INLINEABLE hGetUtf8Lenient #-}

-- | 'hGet' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetUtf8ThrowM ::
  ( HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8ThrowM h = hGet h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetUtf8ThrowM #-}

-- | 'hGetSome' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetSomeUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetSomeUtf8 h = fmap decodeUtf8 . hGetSome h
{-# INLINEABLE hGetSomeUtf8 #-}

-- | 'hGetSome' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetSomeUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8Lenient h = fmap decodeUtf8Lenient . hGetSome h
{-# INLINEABLE hGetSomeUtf8Lenient #-}

-- | 'hGetSome' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetSomeUtf8ThrowM ::
  ( HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8ThrowM h = hGetSome h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetSomeUtf8ThrowM #-}

-- | 'hGetNonBlocking' that attempts a UTF-8 conversion.
--
-- @since 0.1
hGetNonBlockingUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap decodeUtf8 . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8 #-}

-- | 'hGetNonBlocking' that converts to UTF-8 in lenient mode.
--
-- @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8Lenient h = fmap decodeUtf8Lenient . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8Lenient #-}

-- | 'hGetNonBlocking' that throws 'UnicodeException' if UTF-8 conversion fails.
--
-- @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HasCallStack,
    MonadHandleReader m,
    MonadThrow m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetNonBlockingUtf8ThrowM #-}
