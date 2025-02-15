-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.FileWriter
  ( -- * Effect
    MonadFileWriter (..),
    OsPath,

    -- * UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,
    FS.UTF8.encodeUtf8,

    -- * Reexports
    ByteString,
    Text,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as FS.UTF8
import GHC.Stack (HasCallStack)

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadFileWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeBinaryFile :: (HasCallStack) => OsPath -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendBinaryFile :: (HasCallStack) => OsPath -> ByteString -> m ()

-- | @since 0.1
instance MonadFileWriter IO where
  writeBinaryFile = FS.IO.writeBinaryFileIO
  {-# INLINEABLE writeBinaryFile #-}
  appendBinaryFile = FS.IO.appendBinaryFileIO
  {-# INLINEABLE appendBinaryFile #-}

-- | @since 0.1
instance (MonadFileWriter m) => MonadFileWriter (ReaderT env m) where
  writeBinaryFile p = lift . writeBinaryFile p
  {-# INLINEABLE writeBinaryFile #-}
  appendBinaryFile p = lift . appendBinaryFile p
  {-# INLINEABLE appendBinaryFile #-}

-- | Writes to a file as UTF-8.
--
-- @since 0.1
writeFileUtf8 :: (HasCallStack, MonadFileWriter m) => OsPath -> Text -> m ()
writeFileUtf8 p = writeBinaryFile p . FS.UTF8.encodeUtf8
{-# INLINEABLE writeFileUtf8 #-}

-- | Appends to a file as UTF-8.
--
-- @since 0.1
appendFileUtf8 :: (HasCallStack, MonadFileWriter m) => OsPath -> Text -> m ()
appendFileUtf8 p = appendBinaryFile p . FS.UTF8.encodeUtf8
{-# INLINEABLE appendFileUtf8 #-}
