{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.FileSystem.UTF8
  ( -- * Decoding UTF-8

    -- ** Total
    decodeUtf8,
    decodeUtf8Lenient,

    -- ** Partial
    decodeUtf8ThrowM,
    decodeUtf8Fail,
    unsafeDecodeUtf8,

    -- * Encoding UTF-8
    TEnc.encodeUtf8,
  )
where

import Control.Exception (Exception (displayException))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.Exception (MonadThrow, throwM)
import GHC.Stack (HasCallStack)

-- NOTE: -Wno-redundant-constraints is because the HasCallStack is redundant
-- on some of these functions when the exceptions library is too old.
-- Disabling the warning is easier than trying to get it right with cpp.

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
  (HasCallStack, MonadThrow m) =>
  ByteString ->
  m Text
decodeUtf8ThrowM =
  decodeUtf8 >.> \case
    Right txt -> pure txt
    Left ex -> throwM ex

-- | Decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8Fail ::
  (MonadFail m) =>
  ByteString ->
  m Text
decodeUtf8Fail =
  decodeUtf8 >.> \case
    Right txt -> pure txt
    Left ex -> fail $ displayException ex

-- | Decodes a 'ByteString' to UTF-8.
--
-- __WARNING: Partial__
--
-- @since 0.1
unsafeDecodeUtf8 ::
  (HasCallStack) =>
  ByteString ->
  Text
unsafeDecodeUtf8 =
  decodeUtf8 >.> \case
    Right txt -> txt
    Left ex -> error $ displayException ex

-- | Flipped '(.)'.
--
-- @since 0.1
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
