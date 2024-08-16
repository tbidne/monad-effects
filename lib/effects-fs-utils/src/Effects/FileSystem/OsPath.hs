{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.FileSystem.OsPath
  ( -- * Types
    OsPath,

    -- * Encoding

    -- ** Total
    encode,
    encodeLenient,

    -- ** Partial
    encodeThrowM,
    encodeFail,
    unsafeEncode,

    -- * Encoding + Validation

    -- ** Total
    osp,
    ospPathSep,
    encodeValid,
    encodeValidLenient,

    -- ** Partial
    encodeValidThrowM,
    encodeValidFail,
    unsafeEncodeValid,

    -- * Decoding

    -- ** Total
    decode,
    decodeLenient,
    decodeDisplayEx,
    decodeShow,

    -- ** Partial
    decodeThrowM,
    decodeFail,
    unsafeDecode,

    -- * Functions
    (</>),
    (<.>),
    (-<.>),

    -- * Legacy
    (</>!),
    (!</>),
    combineFilePaths,
  )
where

import Control.Exception (Exception (displayException))
import Effects.Exception (MonadThrow, throwM)
import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import System.FilePath qualified as FP
import System.IO (TextEncoding)
import System.IO qualified as IO
import System.OsPath (OsPath, osp, (-<.>), (<.>), (</>))
import System.OsPath qualified as OsPath
import System.OsPath.Encoding (EncodingException (EncodingError))

-- NOTE: -Wno-redundant-constraints is because the HasCallStack is redundant
-- on some of these functions when the exceptions library is too old.
-- Disabling the warning is easier than trying to get it right with cpp.

{- ORMOLU_DISABLE -}

-- | Like 'osp', except it runs paths through a "replace function" first.
-- On posix, replaces '\\' with '/'. On windows, does the opposite.
--
-- This is convenient for writing paths in a platform-agnostic way i.e. we
-- are expecting a path
--
-- @
--   "path\/to\/foo" -- unix
--   "path\\to\\foo" -- windows
-- @
--
-- The normal way to handle this would be to use the combine function '(</>)'
-- i.e.
--
-- @
--   [osp|path|] </> [osp|to|] </> [osp|foo|]
-- @
--
-- This can be quite cumbersome for long paths, so we provide this alternative,
-- allowing:
--
-- @
-- [ospPathSep|path\/to\/foo]
-- @
--
-- Which will automatically convert slashes.
ospPathSep :: QuasiQuoter
ospPathSep =
  QuasiQuoter
    { quoteExp = osp.quoteExp . replaceSlashes,
      quotePat = osp.quotePat . replaceSlashes,
      quoteType = osp.quoteType . replaceSlashes,
      quoteDec = osp.quoteDec . replaceSlashes
    }
  where
    replaceSlashes :: FilePath -> FilePath
    replaceSlashes = foldr go ""
      where
#if WINDOWS
        go '/' acc = '\\' : acc
#else
        go '\\' acc = '/' : acc
#endif
        go c acc = c : acc

{- ORMOLU_ENABLE -}

-- | Encodes a 'FilePath' to an 'OsPath'. This is a pure version of filepath's
-- 'OsPath.encodeUtf' that returns the 'EncodingException' in the event of an
-- error.
--
-- @since 0.1
encode :: FilePath -> Either EncodingException OsPath
encode = OsPath.encodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match.
--
-- @since 0.1
encodeLenient :: FilePath -> OsPath
encodeLenient = elimEx . OsPath.encodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | 'encode' that __also__ checks 'OsPath.isValid' i.e. 'encode'
-- only succeeds if the 'FilePath' can be encoded /and/ passes expected
-- invariants.
--
-- @since 0.1
encodeValid :: FilePath -> Either EncodingException OsPath
encodeValid fp = case encode fp of
  Left ex -> Left ex
  Right op ->
    if OsPath.isValid op
      then Right op
      else Left $ EncodingError (validErr "encodeValid" fp op) Nothing

-- | Total conversion from 'FilePath' to 'OsPath', replacing encode failures
-- with the closest visual match. If the result is not valid, makes it valid.
--
-- @since 0.1
encodeValidLenient :: FilePath -> OsPath
encodeValidLenient = OsPath.makeValid . encodeLenient

-- | 'encode' that throws 'EncodingException'.
--
-- @since 0.1
encodeThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeThrowM =
  encode >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'encodeValid' that throws 'EncodingException'.
--
-- @since 0.1
encodeValidThrowM :: (HasCallStack, MonadThrow m) => FilePath -> m OsPath
encodeValidThrowM fp = case encodeValid fp of
  Left ex -> throwM ex
  Right op -> pure op

-- | 'encodeThrowM' with 'MonadFail'.
--
-- @since 0.1
encodeFail :: (MonadFail m) => FilePath -> m OsPath
encodeFail fp = case encode fp of
  Right txt -> pure txt
  Left ex -> fail $ encodeFailure "encodeFail" fp (displayException ex)

-- | 'encodeValid' with 'MonadFail'.
--
-- @since 0.1
encodeValidFail :: (MonadFail m) => FilePath -> m OsPath
encodeValidFail fp = case encodeValid fp of
  Left ex -> fail $ encodeFailure "encodeValidFail" fp (displayException ex)
  Right op -> pure op

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncode :: (HasCallStack) => FilePath -> OsPath
unsafeEncode fp = case encode fp of
  Left ex ->
    error $ encodeFailure "unsafeEncode" fp (displayException ex)
  Right p -> p

-- | Unsafely converts a 'FilePath' to 'OsPath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeEncodeValid :: (HasCallStack) => FilePath -> OsPath
unsafeEncodeValid fp = case encodeValid fp of
  Left ex ->
    error $ encodeFailure "unsafeEncodeValid" fp (displayException ex)
  Right op -> op

-- | Decodes an 'OsPath' to a 'FilePath'. This is a pure version of filepath's
-- 'OsPath.decodeUtf'.
--
-- @since 0.1
decode :: OsPath -> Either EncodingException FilePath
decode = OsPath.decodeWith utf8Encoder utf16Encoder
  where
    (utf8Encoder, utf16Encoder) = utfEncodings

-- | Total conversion from 'OsPath' to 'FilePath', replacing decode failures
-- with the closest visual match.
--
-- @since 0.1
decodeLenient :: OsPath -> FilePath
decodeLenient = elimEx . OsPath.decodeWith uft8Encoding utf16Encoding
  where
    (uft8Encoding, utf16Encoding, elimEx) = utfEncodingsLenient

-- | 'decode' that throws 'EncodingException'.
--
-- @since 0.1
decodeThrowM :: (HasCallStack, MonadThrow m) => OsPath -> m FilePath
decodeThrowM =
  decode >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | 'decode' with 'MonadFail'.
--
-- @since 0.1
decodeFail :: (MonadFail m) => OsPath -> m FilePath
decodeFail p = case decode p of
  Right txt -> pure txt
  Left ex ->
    fail $ decodeFailure "decodeFail" p (displayException ex)

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, displays
-- the exception.
--
-- @since 0.1
decodeDisplayEx :: OsPath -> String
decodeDisplayEx p = case decode p of
  Left ex -> decodeFailure "decodeDisplayEx" p (displayException ex)
  Right s -> s

-- | Total conversion from 'OsPath' to 'String'. If decoding fails, falls back
-- to its 'Show' instance.
--
-- @since 0.1
decodeShow :: OsPath -> String
decodeShow p = case decode p of
  Left _ -> show p
  Right s -> s

-- | Unsafely converts an 'OsPath' to a 'FilePath' falling back to 'error'.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecode :: (HasCallStack) => OsPath -> FilePath
unsafeDecode p = case decode p of
  Left ex -> error $ decodeFailure "unsafeDecode" p (displayException ex)
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
    UTF16.mkUTF16le TransliterateCodingFailure,
    elimEx
  )
  where
    elimEx = either (error . show) id

decodeFailure :: String -> OsPath -> String -> String
decodeFailure fnName p msg =
  mconcat
    [ "[Effects.FileSystem.OsPath.",
      fnName,
      "]: Could not decode ospath '",
      decodeShow p,
      "' to filepath: '",
      msg,
      "'"
    ]

encodeFailure :: String -> FilePath -> String -> String
encodeFailure fnName fp msg =
  mconcat
    [ "[Effects.FileSystem.OsPath.",
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
    [ "[Effects.FileSystem.OsPath.",
      fnName,
      "]: Original path '",
      fp,
      "' encoded as '",
      decodeShow x,
      "' failed isValid"
    ]

-- | Unsafely combines an 'OsPath' and a 'FilePath' via (</>) with
-- 'unsafeEncode'.
--
-- __WARNING: Partial__
--
-- @since 0.1
(</>!) :: (HasCallStack) => OsPath -> FilePath -> OsPath
p </>! fp = p </> unsafeEncode fp

infixl 9 </>!

-- | Unsafely combines a 'FilePath' and an 'OsPath' via (</>) with
-- 'unsafeEncode'.
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

-- | Flipped '(.)'.
--
-- @since 0.1
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
