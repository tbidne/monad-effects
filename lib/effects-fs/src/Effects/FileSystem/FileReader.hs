-- | Provides the MonadFileReader effect.
--
-- @since 0.1
module Effects.FileSystem.FileReader
  ( -- * Effect
    MonadFileReader (..),
    OsPath,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    FS.UTF8.decodeUtf8,
    FS.UTF8.decodeUtf8Lenient,
    FS.UTF8.decodeUtf8ThrowM,

    -- * Reexports
    ByteString,
    Text,
    UnicodeException,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Effects.Exception (MonadThrow, addCS)
import Effects.FileSystem.IO qualified as FS.IO
import Effects.FileSystem.OsPath (OsPath)
import Effects.FileSystem.UTF8 qualified as FS.UTF8
import GHC.Stack (HasCallStack)

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadFileReader m where
  -- | Reads a file.
  --
  -- @since 0.1
  readBinaryFile :: (HasCallStack) => OsPath -> m ByteString

-- | @since 0.1
instance MonadFileReader IO where
  readBinaryFile = addCS . FS.IO.readBinaryFileIO
  {-# INLINEABLE readBinaryFile #-}

-- | @since 0.1
instance (MonadFileReader m) => MonadFileReader (ReaderT e m) where
  readBinaryFile = lift . readBinaryFile
  {-# INLINEABLE readBinaryFile #-}

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( HasCallStack,
    MonadFileReader m
  ) =>
  OsPath ->
  m (Either UnicodeException Text)
readFileUtf8 = fmap FS.UTF8.decodeUtf8 . readBinaryFile
{-# INLINEABLE readFileUtf8 #-}

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( HasCallStack,
    MonadFileReader m
  ) =>
  OsPath ->
  m Text
readFileUtf8Lenient = fmap FS.UTF8.decodeUtf8Lenient . readBinaryFile
{-# INLINEABLE readFileUtf8Lenient #-}

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m Text
readFileUtf8ThrowM = readBinaryFile >=> FS.UTF8.decodeUtf8ThrowM
{-# INLINEABLE readFileUtf8ThrowM #-}
