-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.FileWriter
  ( -- * Effect
    MonadFileWriter (..),
    Path,

    -- * Text Utils
    writeFileUtf8,
    writeFileLatin1,
    appendFileUtf8,
    appendFileLatin1,
    encodeUtf8,
    encodeLatin1,

    -- * Reexports
    ByteString,
    Text,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Effects.Exception (addCS)
import Effects.FileSystem.Path (Path, appendBinaryFileIO, writeBinaryFileIO)
import GHC.Stack (HasCallStack)

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadFileWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeBinaryFile :: (HasCallStack) => Path -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendBinaryFile :: (HasCallStack) => Path -> ByteString -> m ()

-- | @since 0.1
instance MonadFileWriter IO where
  writeBinaryFile p = addCS . writeBinaryFileIO p
  {-# INLINEABLE writeBinaryFile #-}
  appendBinaryFile p = addCS . appendBinaryFileIO p
  {-# INLINEABLE appendBinaryFile #-}

-- | @since 0.1
instance (MonadFileWriter m) => MonadFileWriter (ReaderT env m) where
  writeBinaryFile p = lift . writeBinaryFile p
  {-# INLINEABLE writeBinaryFile #-}
  appendBinaryFile p = lift . appendBinaryFile p
  {-# INLINEABLE appendBinaryFile #-}

-- | Encodes a 'Text' to UTF-8 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Encodes a 'Text' to Latin-1 'ByteString'.
--
-- @since 0.1
encodeLatin1 :: Text -> ByteString
encodeLatin1 = Char8.pack . T.unpack

-- | Writes to a file as UTF-8.
--
-- @since 0.1
writeFileUtf8 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
writeFileUtf8 p = writeBinaryFile p . encodeUtf8
{-# INLINEABLE writeFileUtf8 #-}

-- | Writes to a file as Latin-1.
--
-- @since 0.1
writeFileLatin1 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
writeFileLatin1 p = writeBinaryFile p . encodeLatin1
{-# INLINEABLE writeFileLatin1 #-}

-- | Appends to a file as UTF-8.
--
-- @since 0.1
appendFileUtf8 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
appendFileUtf8 p = appendBinaryFile p . encodeUtf8
{-# INLINEABLE appendFileUtf8 #-}

-- | Appends to a file as Latin-1.
--
-- @since 0.1
appendFileLatin1 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
appendFileLatin1 p = appendBinaryFile p . encodeLatin1
{-# INLINEABLE appendFileLatin1 #-}
