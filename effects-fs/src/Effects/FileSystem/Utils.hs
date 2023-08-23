-- | Provides filesystem utilities.
--
-- @since 0.1
module Effects.FileSystem.Utils
  ( -- * File paths
    OsPath,

    -- ** To OsPath
    osp,
    toOsPath,
    toOsPathThrowM,
    toOsPathFail,
    unsafeToOsPath,

    -- *** Validation
    toValidOsPath,
    toValidOsPathThrowM,
    toValidOsPathFail,
    unsafeToValidOsPath,

    -- ** From OsPath
    fromOsPath,
    fromOsPathThrowM,
    fromOsPathFail,
    unsafeFromOsPath,

    -- ** Functions
    (</>),
    (</>!),
    (!</>),
    (<</>>!),
    (!<</>>),

    -- ** Legacy
    combineFilePaths,

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

import Control.Exception (Exception (displayException))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, throwM)
import System.FilePath qualified as FP
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException (EncodingError))

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
-- 'OsPath.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
toOsPath :: FilePath -> Either EncodingException OsPath
toOsPath = OsPath.encodeWith IO.utf8 IO.utf16le

-- | 'toOsPath' that __also__ checks 'OsPath.isValid' i.e. 'toOsPath'
-- only succeeds if that 'FilePath' can be encoded /and/ passes expected
-- invariants.
--
-- @since 0.1
toValidOsPath :: FilePath -> Either EncodingException OsPath
toValidOsPath fp = case OsPath.encodeWith IO.utf8 IO.utf16le fp of
  Left ex -> Left ex
  Right op ->
    if OsPath.isValid op
      then Right op
      else Left $ EncodingError (validErr op) Nothing
  where
    validErr x =
      mconcat
        [ "Original path ",
          show fp,
          " encoded as ",
          show x,
          " failed isValid"
        ]

-- | 'toOsPath' that throws 'EncodingException'.
--
-- @since 0.1
toOsPathThrowM :: (MonadThrow m) => FilePath -> m OsPath
toOsPathThrowM =
  toOsPath >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'toValidOsPath' that throws 'EncodingException'.
--
-- @since 0.1
toValidOsPathThrowM :: (MonadThrow m) => FilePath -> m OsPath
toValidOsPathThrowM =
  toValidOsPath >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'toOsPathThrowM' with 'MonadFail'.
--
-- @since 0.1
toOsPathFail :: (MonadFail m) => FilePath -> m OsPath
toOsPathFail =
  toOsPath >.> \case
    Right txt -> pure txt
    Left ex -> fail $ displayException ex

-- | 'toValidOsPath' with 'MonadFail'.
--
-- @since 0.1
toValidOsPathFail :: (MonadFail m) => FilePath -> m OsPath
toValidOsPathFail =
  toValidOsPath >.> \case
    Right txt -> pure txt
    Left ex -> fail $ displayException ex

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeToOsPath :: FilePath -> OsPath
unsafeToOsPath fp = case toOsPath fp of
  Left ex ->
    error $
      mconcat
        [ "Could not convert filepath ",
          show fp,
          " to ospath: ",
          displayException ex
        ]
  Right p -> p

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeToValidOsPath :: FilePath -> OsPath
unsafeToValidOsPath fp = case toValidOsPath fp of
  Left ex ->
    error $
      mconcat
        [ "Could not convert filepath ",
          show fp,
          " to ospath: ",
          displayException ex
        ]
  Right p -> p

-- | Decodes an 'OsPath' to a 'FilePath'. This is a pure version of filepath's
-- 'OsPath.decodeUtf'.
--
-- @since 0.1
fromOsPath :: OsPath -> Either EncodingException FilePath
fromOsPath = OsPath.decodeWith IO.utf8 IO.utf16le

-- | 'fromOsPath' that throws 'EncodingException'.
--
-- @since 0.1
fromOsPathThrowM :: (MonadThrow m) => OsPath -> m FilePath
fromOsPathThrowM =
  fromOsPath >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'fromOsPath' with 'MonadFail'.
--
-- @since 0.1
fromOsPathFail :: (MonadFail m) => OsPath -> m FilePath
fromOsPathFail =
  fromOsPath >.> \case
    Right txt -> pure txt
    Left ex -> fail $ displayException ex

-- | Unsafely converts an 'OsPath' to a 'FilePath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeFromOsPath :: OsPath -> FilePath
unsafeFromOsPath p = case fromOsPath p of
  Left ex ->
    error $
      mconcat
        [ "Could not convert ospath ",
          show p,
          " to filepath: ",
          displayException ex
        ]
  Right fp -> fp

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

-- | Combines an 'OsPath' and a 'FilePath' via (</>), potentially throwing
-- 'EncodingException'. Like '(</>!)', exception uses 'MonadThrow' over 'error'.
--
-- @since 0.1
(<</>>!) :: (MonadThrow m) => OsPath -> FilePath -> m OsPath
p <</>>! fp = do
  fp' <- toOsPathThrowM fp
  pure $ p </> fp'

infixl 9 <</>>!

-- | Combines a 'FilePath' and an 'OsPath' via (</>), potentially throwing
-- 'EncodingException'. Like '(!</>)', exception uses 'MonadThrow' over 'error'.
--
-- @since 0.1
(!<</>>) :: (MonadThrow m) => FilePath -> OsPath -> m OsPath
(!<</>>) = flip (<</>>!)

infixl 9 !<</>>

-- | Unsafely combines an 'OsPath' and a 'FilePath' via (</>) with
-- 'unsafeToOsPath'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(</>!) :: OsPath -> FilePath -> OsPath
p </>! fp = p </> unsafeToOsPath fp

infixl 9 </>!

-- | Unsafely combines a 'FilePath' and an 'OsPath' via (</>) with
-- 'unsafeToOsPath'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(!</>) :: FilePath -> OsPath -> OsPath
(!</>) = flip (</>!)

infixl 9 !</>

-- | Legacy alias for FilePaths' </> operator. Exists because the </> exported
-- here is @'(</>)' :: 'OsPath' -> 'OsPath' -> 'OsPath'@.
--
-- @since 0.1
combineFilePaths :: FilePath -> FilePath -> FilePath
combineFilePaths = (FP.</>)

-- | Flipped '(.)'.
--
-- @since 0.1
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
