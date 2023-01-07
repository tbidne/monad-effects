-- | Provides the MonadFileWriter effect.
--
-- @since 0.1
module Effects.FileSystem.MonadFileWriter
  ( -- * Class
    MonadFileWriter (..),
    Path,

    -- * UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,
    encodeUtf8,

    -- * Reexports
    ByteString,
    Text,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Effects.FileSystem.Types (Path)
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Stack (HasCallStack)

-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => MonadFileWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeBinaryFile :: HasCallStack => Path -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendBinaryFile :: HasCallStack => Path -> ByteString -> m ()

-- | @since 0.1
instance MonadFileWriter IO where
  writeBinaryFile f = addCallStack . BS.writeFile f
  appendBinaryFile f = addCallStack . BS.appendFile f

-- | @since 0.1
instance MonadFileWriter m => MonadFileWriter (ReaderT env m) where
  writeBinaryFile f = lift . writeBinaryFile f
  appendBinaryFile f = lift . appendBinaryFile f

-- | Encodes a 'Text' to 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Writes to a file.
--
-- @since 0.1
writeFileUtf8 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
writeFileUtf8 f = writeBinaryFile f . encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 :: (HasCallStack, MonadFileWriter m) => Path -> Text -> m ()
appendFileUtf8 f = appendBinaryFile f . encodeUtf8
