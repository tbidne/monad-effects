module Effects.HTTP.Client.Internal
  ( -- * Exceptions,
    NetworkStatusE (..),
    NetworkReadBodyE (..),
    NetworkDecodeUtf8E (..),
    NetworkDecodeJsonE (..),

    -- * Readers
    readResponse,

    -- ** Decoders
    decodeJson,
    decodeUtf8,

    -- * Misc
    getStatusCode,
    mapThrowLeft,
  )
where

import Control.Exception (Exception, SomeException, displayException)
import Control.Exception.Utils (MonadCatch, MonadThrow, throwM, trySync)
import Control.Monad (when)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Asn
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import GHC.Stack.Types (HasCallStack)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

-- | Helper for reading a response, checking for status 200 and exceptions
-- thrown by the consumer.
--
-- @since 0.1
readResponse ::
  ( HasCallStack,
    MonadCatch m
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response br ->
  -- | Consumer.
  (br -> m a) ->
  m a
readResponse url res consumer = do
  let bodyReader = HttpClient.responseBody res
      status = HttpClient.responseStatus res
      statusCode = getStatusCode res

  when (statusCode /= 200) $
    throwM $
      MkNetworkStatusE url status

  mapThrowLeft
    (MkNetworkReadBodyE url)
    =<< trySync (consumer bodyReader)

decodeJson ::
  ( FromJSON a,
    MonadThrow m
  ) =>
  [ByteString] ->
  m a
decodeJson bodyBs = do
  mapThrowLeft
    (MkNetworkDecodeJsonE bs)
    (Asn.eitherDecodeStrict bs)
  where
    bs = mconcat bodyBs

decodeUtf8 ::
  (MonadThrow m) =>
  [ByteString] ->
  m Text
decodeUtf8 bodyBs = do
  mapThrowLeft
    (MkNetworkDecodeUtf8E bs)
    $ TEnc.decodeUtf8' bs
  where
    bs = mconcat bodyBs

data NetworkStatusE = MkNetworkStatusE String Status
  deriving stock (Show)

instance Exception NetworkStatusE where
  displayException (MkNetworkStatusE url status) =
    mconcat
      [ "Received ",
        show $ Status.statusCode status,
        " for url '",
        url,
        "': ",
        statusMessage status
      ]

data NetworkReadBodyE = MkNetworkReadBodyE String SomeException
  deriving stock (Show)

instance Exception NetworkReadBodyE where
  displayException (MkNetworkReadBodyE url ex) =
    mconcat
      [ "Exception reading body for url '",
        url,
        "':\n\n",
        displayException ex
      ]

data NetworkDecodeUtf8E = MkNetworkDecodeUtf8E ByteString UnicodeException
  deriving stock (Show)

instance Exception NetworkDecodeUtf8E where
  displayException (MkNetworkDecodeUtf8E bs err) =
    mconcat
      [ "Could not decode UTF-8: ",
        displayException err,
        ". Bytes: ",
        show bs
      ]

data NetworkDecodeJsonE = MkNetworkDecodeJsonE ByteString String
  deriving stock (Show)

instance Exception NetworkDecodeJsonE where
  displayException (MkNetworkDecodeJsonE jsonBs err) =
    mconcat
      [ "Could not decode JSON: ",
        err,
        ". Bytes: ",
        show jsonBs
      ]

statusMessage :: Status -> String
statusMessage s =
  mconcat
    [ "Status message: ",
      show $ Status.statusMessage s
    ]

getStatusCode :: Response body -> Int
getStatusCode = Status.statusCode . HttpClient.responseStatus

mapThrowLeft :: (Exception e2, MonadThrow m) => (e1 -> e2) -> Either e1 a -> m a
mapThrowLeft f = throwLeft . first f

throwLeft :: (Exception e, MonadThrow m) => Either e a -> m a
throwLeft (Right x) = pure x
throwLeft (Left e) = throwM e
