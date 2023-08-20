-- | Provides filesystem utilities.
--
-- @since 0.1
module Effects.FileSystem.Utils
  ( -- * File paths
    OsPath,
    toOsPath,
    toOsPathThrowM,
    fromOsPath,
    fromOsPathThrowM,
    (</>),

    -- * IO actions
    readBinaryFileIO,
    writeBinaryFileIO,
    appendBinaryFileIO,
    openBinaryFileIO,
    withBinaryFileIO,

    -- * Decoding UTF-8
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

    -- * Encoding UTF-8
    TEnc.encodeUtf8,

    -- * Misc
    (>.>),
  )
where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, throwM)
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import System.OsPath (OsPath, (</>))
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException)

-- NOTE: decodeUtf vs. decodeFs
--
-- The latter (decodeFs) is closer to what base used to do, so using it
-- would most closely keep the previous semantics. So why do we use decodeUtf
-- instead? Because the latter relies on the environment locale and seems
-- more likely to cause strange errors. See the haddocks and also the
-- following blog post.
--
-- https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html

-- | Encodes a 'FilePath' to an 'OsPath'. This is a pure version of filepath's
-- 'OsPath.encodeUtf'.
--
-- @since 0.1
toOsPath :: FilePath -> Either EncodingException OsPath
toOsPath = OsPath.encodeWith IO.utf8 IO.utf16le

-- | 'toOsPath' that throws 'EncodingException'. Equivalent to
-- 'OsPath.encodeUtf'.
--
-- @since 0.1
toOsPathThrowM :: (MonadThrow m) => FilePath -> m OsPath
toOsPathThrowM =
  toOsPath >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Decodes an 'OsPath' to a 'FilePath'. This is a pure version of filepath's
-- 'OsPath.decodeUtf'.
--
-- @since 0.1
fromOsPath :: OsPath -> Either EncodingException FilePath
fromOsPath = OsPath.decodeWith IO.utf8 IO.utf16le

-- | 'fromOsPath' that throws 'EncodingException'. Equivalent to
-- 'OsPath.decodeUtf'.
--
-- @since 0.1
fromOsPathThrowM :: (MonadThrow m) => OsPath -> m FilePath
fromOsPathThrowM =
  fromOsPath >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | @since 0.1
readBinaryFileIO :: OsPath -> IO ByteString
readBinaryFileIO = fromOsPathThrowM >=> BS.readFile

-- | @since 0.1
writeBinaryFileIO :: OsPath -> ByteString -> IO ()
writeBinaryFileIO p bs = fromOsPathThrowM p >>= \p' -> BS.writeFile p' bs

-- | @since 0.1
appendBinaryFileIO :: OsPath -> ByteString -> IO ()
appendBinaryFileIO p bs = fromOsPathThrowM p >>= \p' -> BS.appendFile p' bs

-- | @since 0.1
openBinaryFileIO :: OsPath -> IOMode -> IO Handle
openBinaryFileIO p m = fromOsPathThrowM p >>= \h -> IO.openBinaryFile h m

-- | @since 0.1
withBinaryFileIO :: OsPath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO p m f = fromOsPathThrowM p >>= \h -> IO.withBinaryFile h m f

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
  (MonadThrow m) =>
  ByteString ->
  m Text
decodeUtf8ThrowM =
  TEnc.decodeUtf8' >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Flipped '(.)'.
--
-- @since 0.1
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
