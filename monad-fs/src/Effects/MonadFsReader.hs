{-# LANGUAGE CPP #-}

-- | Provides the FileSystem effect.
--
-- @since 0.1
module Effects.MonadFsReader
  ( -- * Class
    MonadFsReader (..),
    Path,

    -- * UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

    -- * Xdg Utils
    getXdgConfig,

    -- * Reexports
    ByteString,
    Text,
    UnicodeException,
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
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack, throwWithCallStack),
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Directory (XdgDirectory (XdgConfig))
#if MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath (OsPath)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif
import Prelude hiding (appendFile, readFile, writeFile)

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
  getXdgDirectory :: HasCallStack => XdgDirectory -> Path -> m Path

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
  getFileSize = addCallStack . fmap fromIntegral . Dir.getFileSize
  getHomeDirectory = addCallStack Dir.getHomeDirectory
  getXdgDirectory xdg = addCallStack . Dir.getXdgDirectory xdg
  readFile = addCallStack . BS.readFile
  doesFileExist = addCallStack . Dir.doesFileExist
  doesDirectoryExist = addCallStack . Dir.doesDirectoryExist
  doesPathExist = addCallStack . Dir.doesPathExist
  canonicalizePath = addCallStack . Dir.canonicalizePath
  listDirectory = addCallStack . Dir.listDirectory

-- | @since 0.1
instance MonadFsReader m => MonadFsReader (ReaderT e m) where
  getFileSize = lift . getFileSize
  getHomeDirectory = lift getHomeDirectory
  getXdgDirectory xdg = lift . getXdgDirectory xdg
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

-- | Leniently decodes a 'ByteString' to UTF-8.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TEnc.decodeUtf8With TEncError.lenientDecode

-- | Decodes a 'ByteString' to UTF-8. Can throw 'UnicodeException'.
--
-- @since 0.1
decodeUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m
  ) =>
  ByteString ->
  m Text
decodeUtf8ThrowM =
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

-- | Decodes a file as UTF-8. Throws 'UnicodeException' for decode errors.
--
-- @since 0.1
readFileUtf8ThrowM ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFsReader m
  ) =>
  Path ->
  m Text
readFileUtf8ThrowM = readFile >=> decodeUtf8ThrowM

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadFsReader m) => Path -> m Path
getXdgConfig = getXdgDirectory XdgConfig

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>
