{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides dynamic network effects.
--
-- @since 0.1
module Effects.HTTP.Client
  ( -- * Effect
    MonadNetwork (..),
    BodyReader,

    -- * Helpers
    readResponseJson,
    readResponseUtf8,
    readResponse,

    -- * Exceptions
    NetworkStatusE (..),
    NetworkReadBodyE (..),
    NetworkDecodeUtf8E (..),
    NetworkDecodeJsonE (..),

    -- * Re-exports
    ManagerSettings,
    Manager,
    Request,
    Response,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (Text)
import Effects.HTTP.Client.Internal
  ( NetworkDecodeJsonE (MkNetworkDecodeJsonE),
    NetworkDecodeUtf8E (MkNetworkDecodeUtf8E),
    NetworkReadBodyE (MkNetworkReadBodyE),
    NetworkStatusE (MkNetworkStatusE),
  )
import Effects.HTTP.Client.Internal qualified as Internal
import GHC.Stack.Types (HasCallStack)
import Network.HTTP.Client
  ( HistoriedResponse,
    Manager,
    ManagerSettings,
    Request,
    Response,
  )
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as Tls

-- | Dynamic effect for network effects.
--
-- @since 0.1
class (Monad m) => MonadNetwork m where
  -- | @since 0.1
  newManager ::
    (HasCallStack) =>
    -- | .
    ManagerSettings ->
    m Manager

  -- | @since 0.1
  newTlsManager ::
    (HasCallStack) =>
    -- | .
    m Manager

  -- | @since 0.1
  newTlsManagerWith ::
    (HasCallStack) =>
    -- | .
    ManagerSettings ->
    m Manager

  -- | @since 0.1
  applyDigestAuth ::
    forall n.
    ( HasCallStack,
      MonadThrow n
    ) =>
    -- | .
    ByteString ->
    ByteString ->
    Request ->
    Manager ->
    m (n Request)

  -- | @since 0.1
  withResponse ::
    (HasCallStack) =>
    -- | .
    Request ->
    Manager ->
    (Response (BodyReader m) -> m a) ->
    m a

  -- | @since 0.1
  withResponseHistory ::
    (HasCallStack) =>
    -- | .
    Request ->
    Manager ->
    (HistoriedResponse (BodyReader m) -> m a) ->
    m a

  -- | @since 0.1
  brRead ::
    (HasCallStack) =>
    -- | .
    BodyReader m ->
    m ByteString

  -- | @since 0.1
  brReadSome ::
    (HasCallStack) =>
    -- | .
    BodyReader m ->
    Int ->
    m LazyByteString

  -- | @since 0.1
  brConsume ::
    (HasCallStack) =>
    -- | .
    BodyReader m ->
    m [ByteString]

instance MonadNetwork IO where
  newManager = Http.newManager
  {-# INLINEABLE newManager #-}
  newTlsManager = Tls.newTlsManager
  {-# INLINEABLE newTlsManager #-}
  newTlsManagerWith = Tls.newTlsManagerWith
  {-# INLINEABLE newTlsManagerWith #-}
  applyDigestAuth = Tls.applyDigestAuth
  {-# INLINEABLE applyDigestAuth #-}
  withResponse = Http.withResponse
  {-# INLINEABLE withResponse #-}
  withResponseHistory = Http.withResponseHistory
  {-# INLINEABLE withResponseHistory #-}
  brRead = Http.brRead
  {-# INLINEABLE brRead #-}
  brReadSome = Http.brReadSome
  {-# INLINEABLE brReadSome #-}
  brConsume = Http.brConsume
  {-# INLINEABLE brConsume #-}

-- | @since 0.1
instance (MonadNetwork m) => MonadNetwork (ReaderT env m) where
  newManager = lift . newManager
  {-# INLINEABLE newManager #-}
  newTlsManager = lift newTlsManager
  {-# INLINEABLE newTlsManager #-}
  newTlsManagerWith = lift . newTlsManagerWith
  {-# INLINEABLE newTlsManagerWith #-}
  applyDigestAuth b1 b2 r = lift . applyDigestAuth b1 b2 r
  {-# INLINEABLE applyDigestAuth #-}
  withResponse r m k = withUnlift $ \unlift ->
    withResponse r m (unlift . k . fmap lift)
  {-# INLINEABLE withResponse #-}
  withResponseHistory r m k = withUnlift $ \unlift ->
    withResponseHistory r m (unlift . k . fmap lift)
  {-# INLINEABLE withResponseHistory #-}
  brRead br = withUnlift $ \unlift -> brRead (unlift br)
  {-# INLINEABLE brRead #-}
  brReadSome br n = withUnlift $ \unlift -> brReadSome (unlift br) n
  {-# INLINEABLE brReadSome #-}
  brConsume br = withUnlift $ \unlift -> brConsume (unlift br)
  {-# INLINEABLE brConsume #-}

-- | @since 0.1
type BodyReader m = m ByteString

withUnlift ::
  forall m env b.
  (Monad m) =>
  ((forall a. ReaderT env m a -> m a) -> m b) ->
  ReaderT env m b
withUnlift action = ask >>= \env -> lift $ action (\m -> runReaderT m env)
{-# INLINEABLE withUnlift #-}

-- | Helper for reading a response, checking for status 200 and exceptions
-- thrown by the consumer.
--
-- @since 0.1
readResponse ::
  forall a m.
  ( HasCallStack,
    MonadCatch m
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader m) ->
  -- | Consumer.
  (BodyReader m -> m a) ->
  m a
readResponse = Internal.readResponse
{-# INLINEABLE readResponse #-}

-- | Helper for reading a response, decoding to UTF-8.
--
-- @since 0.1
readResponseUtf8 ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadNetwork m
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader m) ->
  m Text
readResponseUtf8 url res =
  readResponse url res brConsume >>= Internal.decodeUtf8
{-# INLINEABLE readResponseUtf8 #-}

-- | Helper for reading a response, decoding JSON.
readResponseJson ::
  forall a m.
  ( FromJSON a,
    HasCallStack,
    MonadCatch m,
    MonadNetwork m
  ) =>
  -- | String url.
  String ->
  -- | Response.
  Response (BodyReader m) ->
  m a
readResponseJson url res =
  readResponse url res brConsume >>= Internal.decodeJson
{-# INLINEABLE readResponseJson #-}
