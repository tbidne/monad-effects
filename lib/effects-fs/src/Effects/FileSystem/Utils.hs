{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- 1.4.200.0 inadvertently deprecates some symbols we use. Including this here
-- so CI does not complain.
--
-- https://github.com/haskell/filepath/issues/209
#if MIN_VERSION_filepath(1,4,200) && !MIN_VERSION_filepath(1,5,0)
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

-- | Provides filesystem utilities.
--
-- @since 0.1
module Effects.FileSystem.Utils
  ( -- * File paths
    OsPath,

    -- ** Encoding

    -- *** Total
    encodeFpToOs,
    encodeFpToOsLenient,

    -- *** Partial
    encodeFpToOsThrowM,
    encodeFpToOsFail,
    unsafeEncodeFpToOs,

    -- ** Encoding + Validation

    -- *** Total
    osp,
    encodeFpToValidOs,
    encodeFpToValidOsLenient,

    -- *** Partial
    encodeFpToValidOsThrowM,
    encodeFpToValidOsFail,
    unsafeEncodeFpToValidOs,

    -- ** Decoding

    -- *** Total
    decodeOsToFp,
    decodeOsToFpLenient,
    decodeOsToFpDisplayEx,
    decodeOsToFpShow,

    -- *** Partial
    decodeOsToFpThrowM,
    decodeOsToFpFail,
    unsafeDecodeOsToFp,

    -- ** Functions
    (</>),
    (<.>),
    (-<.>),

    -- ** Legacy
    (</>!),
    (!</>),
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
    throwPathIOError,
    (>.>),
  )
where

import Control.Exception (Exception (displayException))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, throwM)
import Effects.System.PosixCompat qualified as PC
import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import GHC.Stack (HasCallStack)
import System.File.OsPath qualified as FileIO
import System.FilePath qualified as FP
import System.IO (Handle, IOMode, TextEncoding)
import System.IO qualified as IO
import System.IO.Error (IOErrorType)
import System.OsPath (OsPath, osp, (-<.>), (<.>), (</>))
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException (EncodingError))

-- NOTE: -Wno-redundant-constraints is because the HasCallStack is redundant
-- on some of these functions when the exceptions library is too old.
-- Disabling the warning is easier than trying to get it right with cpp.

-- | Encodes a 'FilePath' to an 'OsPath'. This is a pure version of filepath's
-- 'OsPath.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
encodeFpToOs :: FilePath -> Either EncodingException OsPath
encodeFpToOs = OsPath.encodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match.
--
-- @since 0.1
encodeFpToOsLenient :: FilePath -> OsPath
encodeFpToOsLenient = elimEx . OsPath.encodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | 'encodeFpToOs' that __also__ checks 'OsPath.isValid' i.e. 'encodeFpToOs'
-- only succeeds if the 'FilePath' can be encoded /and/ passes expected
-- invariants.
--
-- @since 0.1
encodeFpToValidOs :: FilePath -> Either EncodingException OsPath
encodeFpToValidOs fp = case encodeFpToOs fp of
  Left ex -> Left ex
  Right op ->
    if OsPath.isValid op
      then Right op
      else Left $ EncodingError (validErr "encodeFpToValidOs" fp op) Nothing

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match. If the result is not valid, makes it valid.
--
-- @since 0.1
encodeFpToValidOsLenient :: FilePath -> OsPath
encodeFpToValidOsLenient = OsPath.makeValid . encodeFpToOsLenient

-- | 'encodeFpToOs' that throws 'EncodingException'.
--
-- @since 0.1
encodeFpToOsThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeFpToOsThrowM =
  encodeFpToOs >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'encodeFpToValidOs' that throws 'EncodingException'.
--
-- @since 0.1
encodeFpToValidOsThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeFpToValidOsThrowM fp = case encodeFpToValidOs fp of
  Left ex -> throwM ex
  Right op -> pure op

-- | 'encodeFpToOsThrowM' with 'MonadFail'.
--
-- @since 0.1
encodeFpToOsFail :: (MonadFail m) => FilePath -> m OsPath
encodeFpToOsFail fp = case encodeFpToOs fp of
  Right txt -> pure txt
  Left ex -> fail $ encodeFailure "encodeFpToOsFail" fp (displayException ex)

-- | 'encodeFpToValidOs' with 'MonadFail'.
--
-- @since 0.1
encodeFpToValidOsFail :: (MonadFail m) => FilePath -> m OsPath
encodeFpToValidOsFail fp = case encodeFpToValidOs fp of
  Left ex -> fail $ encodeFailure "encodeFpToValidOsFail" fp (displayException ex)
  Right op -> pure op

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncodeFpToOs :: (HasCallStack) => FilePath -> OsPath
unsafeEncodeFpToOs fp = case encodeFpToOs fp of
  Left ex ->
    error $ encodeFailure "unsafeEncodeFpToOs" fp (displayException ex)
  Right p -> p

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncodeFpToValidOs :: (HasCallStack) => FilePath -> OsPath
unsafeEncodeFpToValidOs fp = case encodeFpToValidOs fp of
  Left ex ->
    error $ encodeFailure "unsafeEncodeFpToValidOs" fp (displayException ex)
  Right op -> op

-- | Decodes an 'OsPath' to a 'FilePath'. This is a pure version of filepath's
-- 'OsPath.decodeUtf'.
--
-- @since 0.1
decodeOsToFp :: OsPath -> Either EncodingException FilePath
decodeOsToFp = OsPath.decodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

-- | Total conversion from 'OsPath' to 'FilePath', replacing decode failures
-- with the closest visual match.
--
-- @since 0.1
decodeOsToFpLenient :: OsPath -> FilePath
decodeOsToFpLenient = elimEx . OsPath.decodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | 'decodeOsToFp' that throws 'EncodingException'.
--
-- @since 0.1
decodeOsToFpThrowM :: (HasCallStack, MonadThrow m) => OsPath -> m FilePath
decodeOsToFpThrowM =
  decodeOsToFp >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'decodeOsToFp' with 'MonadFail'.
--
-- @since 0.1
decodeOsToFpFail :: (MonadFail m) => OsPath -> m FilePath
decodeOsToFpFail p = case decodeOsToFp p of
  Right txt -> pure txt
  Left ex ->
    fail $ decodeFailure "decodeOsToFpFail" p (displayException ex)

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, displays
-- the exception.
--
-- @since 0.1
decodeOsToFpDisplayEx :: OsPath -> String
decodeOsToFpDisplayEx p = case decodeOsToFp p of
  Left ex -> decodeFailure "decodeOsToFpDisplayEx" p (displayException ex)
  Right s -> s

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, falls back
-- to its 'Show' instance.
--
-- @since 0.1
decodeOsToFpShow :: OsPath -> String
decodeOsToFpShow p = case decodeOsToFp p of
  Left _ -> show p
  Right s -> s

-- | Unsafely converts an 'OsPath' to a 'FilePath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecodeOsToFp :: (HasCallStack) => OsPath -> FilePath
unsafeDecodeOsToFp p = case decodeOsToFp p of
  Left ex -> error $ decodeFailure "unsafeDecodeOsToFp" p (displayException ex)
  Right fp -> fp

-- | (UTF8, UTF16LE) encoders.
utfEncodings :: (TextEncoding, TextEncoding)
-- NOTE: [Unix/Windows encodings]
--
-- utf8/utf16le encodings are taken from os-string's encodeUtf implementation.
utfEncodings = (IO.utf8, IO.utf16le)

-- | Like 'utfEncodings' except the encodings are total. We also provide an
-- eliminator for @EncodingException -> a@ (lifted to Either for convenience),
-- because such an @EncodingException@ should be impossible, but the general
-- encode/decode framework returns Either, so we need to handle the impossible
-- Left.
utfEncodingsLenient ::
  ( TextEncoding,
    TextEncoding,
    Either EncodingException a -> a
  )
utfEncodingsLenient =
  ( -- see NOTE: [Unix/Windows encodings]
    --
    -- These encoders are like those defined in utfEncodings, except we use
    -- TransliterateCodingFailure instead of ErrorOnCodingFailure i.e.
    --
    --     mkUTF8/mkUTF16 ErrorOnCodingFailure
    --
    -- These should always succeed.
    UTF8.mkUTF8 TransliterateCodingFailure,
    UTF16.mkUTF16 TransliterateCodingFailure,
    elimEx
  )
  where
    elimEx = either (error . show) id

decodeFailure :: String -> OsPath -> String -> String
decodeFailure fnName p msg =
  mconcat
    [ "[Effects.FileSystem.Utils.",
      fnName,
      "]: Could not decode ospath '",
      decodeOsToFpShow p,
      "' to filepath: '",
      msg,
      "'"
    ]

encodeFailure :: String -> FilePath -> String -> String
encodeFailure fnName fp msg =
  mconcat
    [ "[Effects.FileSystem.Utils.",
      fnName,
      "]: Could not encode filepath '",
      fp,
      "' to ospath: '",
      msg,
      "'"
    ]

validErr :: String -> String -> OsPath -> String
validErr fnName fp x =
  mconcat
    [ "[Effects.FileSystem.Utils.",
      fnName,
      "]: Original path '",
      fp,
      "' encoded as '",
      decodeOsToFpShow x,
      "' failed isValid"
    ]

-- | @since 0.1
readBinaryFileIO :: OsPath -> IO ByteString
readBinaryFileIO = FileIO.readFile'

-- | @since 0.1
writeBinaryFileIO :: OsPath -> ByteString -> IO ()
writeBinaryFileIO = FileIO.writeFile'

-- | @since 0.1
appendBinaryFileIO :: OsPath -> ByteString -> IO ()
appendBinaryFileIO = FileIO.appendFile'

-- | @since 0.1
openBinaryFileIO :: OsPath -> IOMode -> IO Handle
openBinaryFileIO = FileIO.openBinaryFile

-- | @since 0.1
withBinaryFileIO :: OsPath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO = FileIO.withBinaryFile

-- NOTE: notice FileIO.withBinaryFile over FileIO.withBinaryFile' as the
-- latter does _not_ close the handle after use.

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
  decodeUtf8 >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Unsafely combines an 'OsPath' and a 'FilePath' via (</>) with
-- 'unsafeEncodeFpToOs'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(</>!) :: (HasCallStack) => OsPath -> FilePath -> OsPath
p </>! fp = p </> unsafeEncodeFpToOs fp

infixl 9 </>!

-- | Unsafely combines a 'FilePath' and an 'OsPath' via (</>) with
-- 'unsafeEncodeFpToOs'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(!</>) :: (HasCallStack) => FilePath -> OsPath -> OsPath
(!</>) = flip (</>!)

infixl 9 !</>

-- | Legacy alias for FilePaths' </> operator. Exists because the </> exported
-- here is @'(</>)' :: 'OsPath' -> 'OsPath' -> 'OsPath'@.
--
-- @since 0.1
combineFilePaths :: FilePath -> FilePath -> FilePath
combineFilePaths = (FP.</>)

-- | Helper for throwing 'Control.Exception.IOException'.
--
-- @since 0.1
throwPathIOError ::
  (HasCallStack, MonadThrow m) =>
  -- | Path upon which the IO operation failed.
  OsPath ->
  -- | String location (e.g. function name).
  String ->
  -- | Type of exception.
  IOErrorType ->
  -- | Description.
  String ->
  m a
throwPathIOError path = PC.throwPathIOError (decodeOsToFpDisplayEx path)

-- | Flipped '(.)'.
--
-- @since 0.1
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
