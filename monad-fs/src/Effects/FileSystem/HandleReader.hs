-- | Provides the MonadHandleReader effect.
--
-- @since 0.1
module Effects.FileSystem.HandleReader
  ( -- * Effect
    MonadHandleReader (..),

    -- * UTF-8 Utils
    hGetLineUtf8,
    hGetLineUtf8Lenient,
    hGetLineUtf8ThrowM,
    hGetContentsUtf8,
    hGetContentsUtf8Lenient,
    hGetContentsUtf8ThrowM,
    hGetUtf8,
    hGetUtf8Lenient,
    hGetUtf8ThrowM,
    hGetSomeUtf8,
    hGetSomeUtf8Lenient,
    hGetSomeUtf8ThrowM,
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
class Monad m => MonadHandleReader m where
  -- | Lifted 'IO.hIsEOF'.
  --
  -- @since 0.1
  hIsEOF :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hGetBuffering'.
  --
  -- @since 0.1
  hGetBuffering :: HasCallStack => Handle -> m BufferMode

  -- | Lifted 'IO.hIsOpen'.
  --
  -- @since 0.1
  hIsOpen :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hIsClosed'.
  --
  -- @since 0.1
  hIsClosed :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hIsReadable'.
  --
  -- @since 0.1
  hIsReadable :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hIsWritable'.
  --
  -- @since 0.1
  hIsWritable :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hIsSeekable'.
  --
  -- @since 0.1
  hIsSeekable :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hIsTerminalDevice'.
  --
  -- @since 0.1
  hIsTerminalDevice :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hGetEcho'.
  --
  -- @since 0.1
  hGetEcho :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hWaitForInput'.
  --
  -- @since 0.1
  hWaitForInput :: HasCallStack => Handle -> Int -> m Bool

  -- | Lifted 'IO.hReady'.
  --
  -- @since 0.1
  hReady :: HasCallStack => Handle -> m Bool

  -- | Lifted 'IO.hGetChar'.
  --
  -- @since 0.1
  hGetChar :: HasCallStack => Handle -> m Char

  -- | Lifted 'BS.hGetLine'.
  --
  -- @since 0.1
  hGetLine :: HasCallStack => Handle -> m ByteString

  -- | Lifted 'BS.hGetContents'.
  --
  -- @since 0.1
  hGetContents :: HasCallStack => Handle -> m ByteString

  -- | Lifted 'BS.hGet'.
  --
  -- @since 0.1
  hGet :: HasCallStack => Handle -> Int -> m ByteString

  -- | Lifted 'BS.hGetSome'.
  --
  -- @since 0.1
  hGetSome :: HasCallStack => Handle -> Int -> m ByteString

  -- | Lifted 'BS.hGetNonBlocking'.
  --
  -- @since 0.1
  hGetNonBlocking :: HasCallStack => Handle -> Int -> m ByteString

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
instance MonadHandleReader m => MonadHandleReader (ReaderT e m) where
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
