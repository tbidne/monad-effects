module Effects.Notify.Internal.Utils
  ( -- * Text
    maybeStr,
    showt,

    -- * Process
    runProcessIO,

    -- * Int conversions
    unsafeConvertIntegral,

    -- * Misc
    mkProcessText,
    mkProcessTextQuote,
  )
where

import Control.Exception (throwIO)
import Data.Bits (Bits, toIntegralSized)
import Data.Char qualified as Ch
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable
import Effects.System.Process qualified as P
import GHC.IO.Exception
  ( IOErrorType (SystemError),
    IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

-- | Showable to Text.
showt :: (Show a) => a -> Text
showt = T.pack . show

maybeStr :: (IsString s) => (a -> s) -> Maybe a -> s
maybeStr = maybe ""

-- | Runs the string as a shell command, throwing error as exceptions.
runProcessIO ::
  (HasCallStack) =>
  String ->
  IO ()
runProcessIO cmdStr = do
  (ec, out, err) <- P.readCreateProcessWithExitCode pr ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure n ->
      throwIO $
        IOError
          { ioe_handle = Nothing,
            ioe_type = SystemError,
            ioe_location = "runProcessIO",
            ioe_description =
              mconcat
                [ "Running command '",
                  cmdStr,
                  "' failed. Out: '",
                  out,
                  "'; Err: '",
                  err,
                  "'."
                ],
            ioe_errno = Just $ fromIntegral n,
            ioe_filename = Nothing
          }
  where
    pr = P.shell cmdStr

-- | Escapes text so it is acceptable for sending to the shell. If the text
-- is non-empty:
--
-- - Escapes double quotes e.g. " -> \\\"
-- - If there is any whitespace, surrounds text in non-escaped double quotes.
-- - Surrounds text with whitespace.
mkProcessText :: Text -> Text
mkProcessText = mkProcessTextQuote False

-- | Escapes text so it is acceptable for sending to the shell. If the text
-- is non-empty:
--
-- - Escapes double quotes e.g. " -> \\\"
-- - If there is any whitespace, surrounds text in non-escaped double quotes.
-- - Surrounds text with whitespace.
mkProcessTextQuote :: Bool -> Text -> Text
mkProcessTextQuote _ "" = ""
mkProcessTextQuote shouldQuote t =
  TL.toStrict
    . TLB.toLazyText
    . addDQuotes
    $ builder
  where
    (builder, foundWs) = T.foldl' go ("", False) t

    -- Adds quotes if necessary and whitespace unconditionally. We could
    -- use cons/unsnoc (0(1) each) to conditionally add whitespace when
    -- leading/trailing does not already exist, but this is simpler and
    -- harmless.
    --
    -- Also, always quote if the arg is true.
    addDQuotes b =
      if shouldQuote || foundWs
        then " \"" <> b <> "\" "
        else " " <> b <> " "

    go :: (Builder, Bool) -> Char -> (Builder, Bool)
    go (acc, ws) '"' = (acc <> "\\\"", ws)
    go (acc, ws) c = (acc <> TLB.singleton c, Ch.isSpace c || ws)

unsafeConvertIntegral ::
  forall a b.
  ( Bits a,
    Bits b,
    HasCallStack,
    Integral a,
    Integral b,
    Show a,
    Typeable a,
    Typeable b
  ) =>
  a ->
  b
unsafeConvertIntegral x = case convertIntegral x of
  Right y -> y
  Left err -> error err

-- | Like 'fromIntegral', except the conversion is only between integral types.
convertIntegral ::
  forall a b.
  ( Bits a,
    Bits b,
    Integral a,
    Integral b,
    Show a,
    Typeable a,
    Typeable b
  ) =>
  a ->
  Either String b
convertIntegral x = case toIntegralSized x of
  Just y -> Right y
  Nothing ->
    Left $
      mconcat
        [ "convertIntegral: Failed converting ",
          show x,
          " from ",
          show (Typeable.typeOf x),
          " to ",
          show $ Typeable.typeOf (undefined :: b)
        ]
