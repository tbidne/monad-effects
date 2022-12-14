-- | Provides the MonadHandleReader effect.
--
-- @since 0.1
module Effects.FileSystem.MonadHandleReader
  ( -- * Class
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
import Effects.FileSystem.MonadFileReader
  ( decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,
  )
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Stack (HasCallStack)
import System.IO (BufferMode, Handle)
import System.IO qualified as IO

-- | Represents handle reader effects.
--
-- @since 0.1
class Monad m => MonadHandleReader m where
  -- | @since 0.1
  hIsEOF :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hGetBuffering :: HasCallStack => Handle -> m BufferMode

  -- | @since 0.1
  hIsOpen :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hIsClosed :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hIsReadable :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hIsWritable :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hIsSeekable :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hIsTerminalDevice :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hGetEcho :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hWaitForInput :: HasCallStack => Handle -> Int -> m Bool

  -- | @since 0.1
  hReady :: HasCallStack => Handle -> m Bool

  -- | @since 0.1
  hGetChar :: HasCallStack => Handle -> m Char

  -- | @since 0.1
  hGetLine :: HasCallStack => Handle -> m ByteString

  -- | @since 0.1
  hGetContents :: HasCallStack => Handle -> m ByteString

  -- | @since 0.1
  hGet :: HasCallStack => Handle -> Int -> m ByteString

  -- | @since 0.1
  hGetSome :: HasCallStack => Handle -> Int -> m ByteString

  -- | @since 0.1
  hGetNonBlocking :: HasCallStack => Handle -> Int -> m ByteString

-- | @since 0.1
instance MonadHandleReader IO where
  hIsEOF = addCallStack . IO.hIsEOF
  {-# INLINEABLE hIsEOF #-}
  hGetBuffering = addCallStack . IO.hGetBuffering
  {-# INLINEABLE hGetBuffering #-}
  hIsOpen = addCallStack . IO.hIsOpen
  {-# INLINEABLE hIsOpen #-}
  hIsClosed = addCallStack . IO.hIsClosed
  {-# INLINEABLE hIsClosed #-}
  hIsReadable = addCallStack . IO.hIsReadable
  {-# INLINEABLE hIsReadable #-}
  hIsWritable = addCallStack . IO.hIsWritable
  {-# INLINEABLE hIsWritable #-}
  hIsSeekable = addCallStack . IO.hIsSeekable
  {-# INLINEABLE hIsSeekable #-}
  hIsTerminalDevice = addCallStack . IO.hIsTerminalDevice
  {-# INLINEABLE hIsTerminalDevice #-}
  hGetEcho = addCallStack . IO.hGetEcho
  {-# INLINEABLE hGetEcho #-}
  hWaitForInput h = addCallStack . IO.hWaitForInput h
  {-# INLINEABLE hWaitForInput #-}
  hReady = addCallStack . IO.hReady
  {-# INLINEABLE hReady #-}
  hGetChar = addCallStack . IO.hGetChar
  {-# INLINEABLE hGetChar #-}
  hGetLine = addCallStack . BS.hGetLine
  {-# INLINEABLE hGetLine #-}
  hGetContents = addCallStack . BS.hGetContents
  {-# INLINEABLE hGetContents #-}
  hGet h = addCallStack . BS.hGet h
  {-# INLINEABLE hGet #-}
  hGetSome h = addCallStack . BS.hGetSome h
  {-# INLINEABLE hGetSome #-}
  hGetNonBlocking h = addCallStack . BS.hGetNonBlocking h
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

-- | @since 0.1
hGetLineUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetLineUtf8 = fmap decodeUtf8 . hGetLine
{-# INLINEABLE hGetLineUtf8 #-}

-- | @since 0.1
hGetLineUtf8Lenient :: (HasCallStack, MonadHandleReader m) => Handle -> m Text
hGetLineUtf8Lenient = fmap decodeUtf8Lenient . hGetLine
{-# INLINEABLE hGetLineUtf8Lenient #-}

-- | @since 0.1
hGetLineUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetLineUtf8ThrowM = hGetLine >=> decodeUtf8ThrowM
{-# INLINEABLE hGetLineUtf8ThrowM #-}

-- | @since 0.1
hGetContentsUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m (Either UnicodeException Text)
hGetContentsUtf8 = fmap decodeUtf8 . hGetContents
{-# INLINEABLE hGetContentsUtf8 #-}

-- | @since 0.1
hGetContentsUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8Lenient = fmap decodeUtf8Lenient . hGetContents
{-# INLINEABLE hGetContentsUtf8Lenient #-}

-- | @since 0.1
hGetContentsUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleReader m
  ) =>
  Handle ->
  m Text
hGetContentsUtf8ThrowM = hGetContents >=> decodeUtf8ThrowM
{-# INLINEABLE hGetContentsUtf8ThrowM #-}

-- | @since 0.1
hGetUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetUtf8 h = fmap decodeUtf8 . hGet h
{-# INLINEABLE hGetUtf8 #-}

-- | @since 0.1
hGetUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8Lenient h = fmap decodeUtf8Lenient . hGet h
{-# INLINEABLE hGetUtf8Lenient #-}

-- | @since 0.1
hGetUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetUtf8ThrowM h = hGet h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetUtf8ThrowM #-}

-- | @since 0.1
hGetSomeUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetSomeUtf8 h = fmap decodeUtf8 . hGetSome h
{-# INLINEABLE hGetSomeUtf8 #-}

-- | @since 0.1
hGetSomeUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8Lenient h = fmap decodeUtf8Lenient . hGetSome h
{-# INLINEABLE hGetSomeUtf8Lenient #-}

-- | @since 0.1
hGetSomeUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetSomeUtf8ThrowM h = hGetSome h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetSomeUtf8ThrowM #-}

-- | @since 0.1
hGetNonBlockingUtf8 ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m (Either UnicodeException Text)
hGetNonBlockingUtf8 h = fmap decodeUtf8 . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8 #-}

-- | @since 0.1
hGetNonBlockingUtf8Lenient ::
  ( HasCallStack,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8Lenient h = fmap decodeUtf8Lenient . hGetNonBlocking h
{-# INLINEABLE hGetNonBlockingUtf8Lenient #-}

-- | @since 0.1
hGetNonBlockingUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadHandleReader m
  ) =>
  Handle ->
  Int ->
  m Text
hGetNonBlockingUtf8ThrowM h = hGetNonBlocking h >=> decodeUtf8ThrowM
{-# INLINEABLE hGetNonBlockingUtf8ThrowM #-}
