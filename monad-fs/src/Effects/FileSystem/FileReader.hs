-- | Provides the MonadFileReader effect.
--
-- @since 0.1
module Effects.FileSystem.FileReader
  ( -- * Effect
    MonadFileReader (..),
    Path,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

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
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, addCS, throwCS)
import Effects.FileSystem.Path (Path, readBinaryFileIO)
import GHC.Stack (HasCallStack)

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadFileReader m where
  -- | Reads a file.
  --
  -- @since 0.1
  readBinaryFile :: (HasCallStack) => Path -> m ByteString

-- | @since 0.1
instance MonadFileReader IO where
  readBinaryFile = addCS . readBinaryFileIO
  {-# INLINEABLE readBinaryFile #-}

-- | @since 0.1
instance (MonadFileReader m) => MonadFileReader (ReaderT e m) where
  readBinaryFile = lift . readBinaryFile
  {-# INLINEABLE readBinaryFile #-}

-- | Decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8 :: ByteString -> Either UnicodeException Text
decodeUtf8 = TEnc.decodeUtf8'

-- | Leniently decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncError.lenientDecode

-- | Decodes a 'ByteString' to UTF-8. Can throw 'UnicodeException'.
--
-- @since 0.1
decodeUtf8ThrowM ::
  ( HasCallStack,
    MonadThrow m
  ) =>
  ByteString ->
  m Text
decodeUtf8ThrowM =
  TEnc.decodeUtf8' >.> \case
    Right txt -> pure txt
    Left ex -> throwCS ex
{-# INLINEABLE decodeUtf8ThrowM #-}

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( HasCallStack,
    MonadFileReader m
  ) =>
  Path ->
  m (Either UnicodeException Text)
readFileUtf8 = fmap decodeUtf8 . readBinaryFile
{-# INLINEABLE readFileUtf8 #-}

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( HasCallStack,
    MonadFileReader m
  ) =>
  Path ->
  m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . readBinaryFile
{-# INLINEABLE readFileUtf8Lenient #-}

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( HasCallStack,
    MonadFileReader m,
    MonadThrow m
  ) =>
  Path ->
  m Text
readFileUtf8ThrowM = readBinaryFile >=> decodeUtf8ThrowM
{-# INLINEABLE readFileUtf8ThrowM #-}

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>

{-# INLINEABLE (>.>) #-}
