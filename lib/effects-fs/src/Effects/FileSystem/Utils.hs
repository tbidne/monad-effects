-- | Provides filesystem utilities.
--
-- @since 0.1
module Effects.FileSystem.Utils
  ( -- * File paths
    OsPath,

    -- ** To OsPath

    -- *** Encoding
    encodeFpToOs,
    encodeFpToOsThrowM,
    encodeFpToOsFail,
    unsafeEncodeFpToOs,

    -- *** Encoding + Validation
    osp,
    encodeFpToValidOs,
    encodeFpToValidOsThrowM,
    encodeFpToValidOsFail,
    unsafeEncodeFpToValidOs,

    -- ** From OsPath
    osToFp,
    osToText,

    -- *** Decoding
    decodeOsToFp,
    decodeOsToFpThrowM,
    decodeOsToFpFail,
    decodeOsToFpShow,
    decodeOsToFpShowText,
    unsafeDecodeOsToFp,

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
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, throwM)
import GHC.Stack (HasCallStack)
import System.File.OsPath qualified as FileIO
import System.FilePath qualified as FP
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException (EncodingError))

-- | Encodes a 'FilePath' to an 'OsPath'. This is a pure version of filepath's
-- 'OsPath.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
encodeFpToOs :: FilePath -> Either EncodingException OsPath
encodeFpToOs = OsPath.encodeWith IO.utf8 IO.utf16le

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

-- | 'encodeFpToOs' that throws 'EncodingException'.
--
-- @since 0.1
encodeFpToOsThrowM :: (MonadThrow m) => FilePath -> m OsPath
encodeFpToOsThrowM =
  encodeFpToOs >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'encodeFpToValidOs' that throws 'EncodingException'.
--
-- @since 0.1
encodeFpToValidOsThrowM :: (MonadThrow m) => FilePath -> m OsPath
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
decodeOsToFp = OsPath.decodeWith IO.utf8 IO.utf16le

-- | 'decodeOsToFp' that throws 'EncodingException'.
--
-- @since 0.1
decodeOsToFpThrowM :: (MonadThrow m) => OsPath -> m FilePath
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

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, falls back
-- to its 'Show' instance.
--
-- @since 0.1
decodeOsToFpShow :: OsPath -> String
decodeOsToFpShow p = case decodeOsToFp p of
  Left _ -> show p
  Right s -> s

-- | Convenience function for 'T.pack' and 'decodeOsToFpShow'.
--
-- @since 0.1
decodeOsToFpShowText :: OsPath -> Text
decodeOsToFpShowText = T.pack . decodeOsToFpShow

-- | Unsafely converts an 'OsPath' to a 'FilePath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecodeOsToFp :: (HasCallStack) => OsPath -> FilePath
unsafeDecodeOsToFp p = case decodeOsToFp p of
  Left ex -> error $ decodeFailure "unsafeDecodeOsToFp" p (displayException ex)
  Right fp -> fp

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

-- | Combines an 'OsPath' and a 'FilePath' via (</>), potentially throwing
-- 'EncodingException'. Like '(</>!)', exception uses 'MonadThrow' over 'error'.
--
-- @since 0.1
(<</>>!) :: (MonadThrow m) => OsPath -> FilePath -> m OsPath
p <</>>! fp = do
  fp' <- encodeFpToOsThrowM fp
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
-- 'unsafeEncodeFpToOs'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(</>!) :: OsPath -> FilePath -> OsPath
p </>! fp = p </> unsafeEncodeFpToOs fp

infixl 9 </>!

-- | Unsafely combines a 'FilePath' and an 'OsPath' via (</>) with
-- 'unsafeEncodeFpToOs'.
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

-- | Converts 'OsPath' to 'FilePath' without any decoding i.e. the result
-- retains its encoding.
--
-- @since 0.1
osToFp :: OsPath -> FilePath
osToFp = fmap OsPath.toChar . OsPath.unpack

-- | 'Text' version of 'osToFp'.
--
-- @since 0.1
osToText :: OsPath -> Text
osToText = T.pack . osToFp
