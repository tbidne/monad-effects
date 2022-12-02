{-# LANGUAGE CPP #-}

-- | Provides the 'MonadFsReader' typeclass.
--
-- @since 0.1
module Effects.MonadFsReader
  ( -- * Class
    MonadFsReader (..),

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8M,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8M,
    encodeUtf8,
  )
where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Encoding.Error qualified as TEncError
import Effects.MonadCallStack (MonadCallStack (throwWithCallStack),
  checkpointCallStack)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Directory (XdgDirectory (XdgConfig))
#if MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath (OsPath)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif
import Prelude hiding (readFile)

#if MIN_VERSION_directory(1,3,8)
-- | For @directory >= 1.3.8@, 'Path' = 'OsPath'. Below that it is a
-- 'FilePath'.
--
-- @since 0.1
type Path = OsPath
#else
-- | For @directory >= 1.3.8@, 'Path' = 'OsPath'. Below that it is a
-- 'FilePath'.
--
-- @since 0.1
type Path = FilePath
#endif

-- | Represents file-system reader effects.
--
-- @since 0.1
class Monad m => MonadFsReader m where
  -- | Retrieves the file size in bytes.
  --
  -- @since 0.1
  getFileSize :: HasCallStack => Path -> m Natural

  -- | Returns the home directory.
  --
  -- @since 0.1
  getHomeDirectory :: HasCallStack => m Path

  -- | Returns the Xdg config dir.
  --
  -- @since 0.1
  getXdgConfig :: HasCallStack => Path -> m Path

  -- | Reads a file.
  --
  -- @since 0.1
  readFile :: HasCallStack => Path -> m ByteString

  -- | Tests a file's existence.
  --
  -- @since 0.1
  doesFileExist :: HasCallStack => Path -> m Bool

  -- | Tests a directory's existence.
  --
  -- @since 0.1
  doesDirectoryExist :: HasCallStack => Path -> m Bool

  -- | Tests a path's existence.
  --
  -- @since 0.1
  doesPathExist :: HasCallStack => Path -> m Bool

  -- | Canonicalize a path.
  --
  -- @since 0.1
  canonicalizePath :: HasCallStack => Path -> m Path

  -- | Lists a directory.
  --
  -- @since 0.1
  listDirectory :: HasCallStack => Path -> m [Path]

-- | @since 0.1
instance MonadFsReader IO where
  getFileSize = checkpointCallStack . fmap fromIntegral . Dir.getFileSize
  getHomeDirectory = checkpointCallStack Dir.getHomeDirectory
  getXdgConfig = checkpointCallStack . Dir.getXdgDirectory XdgConfig
  readFile = checkpointCallStack . BS.readFile
  doesFileExist = checkpointCallStack . Dir.doesFileExist
  doesDirectoryExist = checkpointCallStack . Dir.doesDirectoryExist
  doesPathExist = checkpointCallStack . Dir.doesPathExist
  canonicalizePath = checkpointCallStack . Dir.canonicalizePath
  listDirectory = checkpointCallStack . Dir.listDirectory

-- | @since 0.1
instance MonadFsReader m => MonadFsReader (ReaderT e m) where
  getFileSize = lift . getFileSize
  getHomeDirectory = lift getHomeDirectory
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  doesPathExist = lift . doesPathExist
  canonicalizePath = lift . canonicalizePath
  listDirectory = lift . listDirectory

-- | Decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8 :: ByteString -> Either UnicodeException Text
decodeUtf8 = TEnc.decodeUtf8'

-- | Encodes a 'Text' to 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Leniently decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncError.lenientDecode

-- | Decodes a 'ByteString' to UTF-8. Can throw 'UnicodeException'.
--
-- @since 0.1
decodeUtf8M ::
  ( HasCallStack,
    MonadCallStack m
  ) =>
  ByteString ->
  m Text
decodeUtf8M =
  TEnc.decodeUtf8' >.> \case
    Right txt -> pure txt
    Left ex -> throwWithCallStack ex

-- | Reads a file as UTF-8.
--
-- @since 0.1
readFileUtf8 ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFsReader m
  ) =>
  Path ->
  m (Either UnicodeException Text)
readFileUtf8 = fmap decodeUtf8 . readFile

-- | Reads a file as UTF-8 in lenient mode.
--
-- @since 0.1
readFileUtf8Lenient ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFsReader m
  ) =>
  Path ->
  m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . readFile

-- | Decodes a file as UTF-8. Can throw 'UnicodeException'.
--
-- @since 0.1
readFileUtf8M ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFsReader m
  ) =>
  Path ->
  m Text
readFileUtf8M = readFile >=> decodeUtf8M

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
