{-# LANGUAGE CPP #-}

-- | Provides the FileSystem effect.
--
-- @since 0.1
module Effects.MonadFs
  ( -- * FileSystem Reader
    MonadFsReader (..),

    -- ** UTF-8 Utils
    readFileUtf8,
    readFileUtf8Lenient,
    readFileUtf8ThrowM,
    decodeUtf8,
    decodeUtf8Lenient,
    decodeUtf8ThrowM,

    -- ** Reexports
    ByteString,
    Text,
    UnicodeException,

    -- * FileSystem Writer
    MonadFsWriter (..),

    -- ** UTF-8 Utils
    writeFileUtf8,
    appendFileUtf8,
    hPutUtf8,
    encodeUtf8,

    -- ** Reexports
    IOMode (..),
    Handle,

    -- * Misc
    Path,
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,
  )
where

import Control.Monad (when, (>=>))
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
import System.IO (Handle, IOMode (..))
import System.IO qualified as IO
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
  getFileSize = addCallStack . fmap fromIntegral . Dir.getFileSize
  getHomeDirectory = addCallStack Dir.getHomeDirectory
  getXdgConfig = addCallStack . Dir.getXdgDirectory XdgConfig
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
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  doesPathExist = lift . doesPathExist
  canonicalizePath = lift . canonicalizePath
  listDirectory = lift . listDirectory

-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => MonadFsWriter m where
  -- | Writes to a file.
  --
  -- @since 0.1
  writeFile :: HasCallStack => Path -> ByteString -> m ()

  -- | Appends to a file.
  --
  -- @since 0.1
  appendFile :: HasCallStack => Path -> ByteString -> m ()

  -- | Opens a file.
  --
  -- @since 0.1
  openFile :: HasCallStack => Path -> IOMode -> m Handle

  -- | Writes to the handle.
  --
  -- @since 0.1
  hPut :: HasCallStack => Handle -> ByteString -> m ()

  -- | Closes a handle.
  --
  -- @since 0.1
  hClose :: HasCallStack => Handle -> m ()

  -- | Flushes a handle.
  --
  -- @since 0.1
  hFlush :: HasCallStack => Handle -> m ()

  -- | Renames a file.
  --
  -- @since 0.1
  renameFile :: HasCallStack => Path -> Path -> m ()

  -- | Removes a file.
  --
  -- @since 0.1
  removeFile :: HasCallStack => Path -> m ()

  -- | Renames a directory.
  --
  -- @since 0.1
  renameDirectory :: HasCallStack => Path -> Path -> m ()

  -- | Removes a directory.
  --
  -- @since 0.1
  removeDirectory :: HasCallStack => Path -> m ()

  -- | Removes a directory recursively.
  --
  -- @since 0.1
  removeDirectoryRecursive :: HasCallStack => Path -> m ()

  -- | Removes a path.
  --
  -- @since 0.1
  removePathForcibly :: HasCallStack => Path -> m ()

  -- | Creates a directory.
  --
  -- @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> Path -> m ()

-- | @since 0.1
instance MonadFsWriter IO where
  writeFile f = addCallStack . BS.writeFile f
  appendFile f = addCallStack . BS.appendFile f
  openFile f = addCallStack . IO.openFile f
  hPut h = addCallStack . BS.hPut h
  hClose = addCallStack . IO.hClose
  hFlush = addCallStack . IO.hFlush
  renameFile f = addCallStack . Dir.renameFile f
  removeFile = addCallStack . Dir.removeFile
  renameDirectory f = addCallStack . Dir.renameDirectory f
  removeDirectory = addCallStack . Dir.removeDirectory
  removeDirectoryRecursive = addCallStack . Dir.removeDirectoryRecursive
  removePathForcibly = addCallStack . Dir.removePathForcibly
  createDirectoryIfMissing b = addCallStack . Dir.createDirectoryIfMissing b

-- | @since 0.1
instance MonadFsWriter m => MonadFsWriter (ReaderT env m) where
  writeFile f = lift . writeFile f
  appendFile f = lift . appendFile f
  openFile f = lift . openFile f
  hPut f = lift . hPut f
  hClose = lift . hClose
  hFlush = lift . hFlush
  renameFile f = lift . renameFile f
  removeFile = lift . removeFile
  renameDirectory f = lift . renameDirectory f
  removeDirectory = lift . removeDirectory
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  removePathForcibly = lift . removePathForcibly
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b

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

-- | Encodes a 'Text' to 'ByteString'.
--
-- @since 0.1
encodeUtf8 :: Text -> ByteString
encodeUtf8 = TEnc.encodeUtf8

-- | Writes to a file.
--
-- @since 0.1
writeFileUtf8 :: (HasCallStack, MonadFsWriter m) => Path -> Text -> m ()
writeFileUtf8 f = writeFile f . encodeUtf8

-- | Appends to a file.
--
-- @since 0.1
appendFileUtf8 :: (HasCallStack, MonadFsWriter m) => Path -> Text -> m ()
appendFileUtf8 f = appendFile f . encodeUtf8

-- | Writes to a handle.
--
-- @since 0.1
hPutUtf8 :: (HasCallStack, MonadFsWriter m) => Handle -> Text -> m ()
hPutUtf8 h = hPut h . encodeUtf8

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    MonadFsReader m,
    MonadFsWriter m
  ) =>
  Path ->
  m ()
removeFileIfExists = removeIfExists doesFileExist removeFile

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( HasCallStack,
    MonadFsReader m,
    MonadFsWriter m
  ) =>
  Path ->
  m ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( HasCallStack,
    MonadFsReader m,
    MonadFsWriter m
  ) =>
  Path ->
  m ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( HasCallStack,
    MonadFsReader m,
    MonadFsWriter m
  ) =>
  Path ->
  m ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixl 9 >.>