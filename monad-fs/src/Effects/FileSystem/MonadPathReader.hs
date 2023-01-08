{-# LANGUAGE CPP #-}

-- | Provides the MonadPathReader effect.
--
-- @since 0.1
module Effects.FileSystem.MonadPathReader
  ( -- * Class
    MonadPathReader (..),
    Path,

    -- * Xdg Utils
    getXdgConfig,

    -- * Reexports
    XdgDirectory (..),
    XdgDirectoryList (..),
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Time (UTCTime (..))
import Effects.FileSystem.Path (Path)
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Directory
  ( Permissions (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif

-- | Represents file-system reader effects.
--
-- @since 0.1
class Monad m => MonadPathReader m where
  -- | @since 0.1
  listDirectory :: HasCallStack => Path -> m [Path]

  -- | @since 0.1
  getDirectoryContents :: HasCallStack => Path -> m [Path]

  -- | @since 0.1
  getCurrentDirectory :: HasCallStack => m Path

  -- | @since 0.1
  getHomeDirectory :: HasCallStack => m Path

  -- | @since 0.1
  getXdgDirectory :: HasCallStack => XdgDirectory -> Path -> m Path

  -- | @since 0.1
  getXdgDirectoryList :: HasCallStack => XdgDirectoryList -> m [Path]

  -- | @since 0.1
  getAppUserDataDirectory :: HasCallStack => Path -> m Path

  -- | @since 0.1
  getUserDocumentsDirectory :: HasCallStack => m Path

  -- | @since 0.1
  getTemporaryDirectory :: HasCallStack => m Path

  -- | @since 0.1
  getFileSize :: HasCallStack => Path -> m Natural

  -- | @since 0.1
  canonicalizePath :: HasCallStack => Path -> m Path

  -- | @since 0.1
  makeAbsolute :: HasCallStack => Path -> m Path

  -- | @since 0.1
  makeRelativeToCurrentDirectory :: HasCallStack => Path -> m Path

  -- | @since 0.1
  doesPathExist :: HasCallStack => Path -> m Bool

  -- | @since 0.1
  doesFileExist :: HasCallStack => Path -> m Bool

  -- | @since 0.1
  doesDirectoryExist :: HasCallStack => Path -> m Bool

  -- | @since 0.1
  findExecutable :: HasCallStack => String -> m (Maybe Path)

  -- | @since 0.1
  findExecutables :: HasCallStack => String -> m [Path]

  -- | @since 0.1
  findExecutablesInDirectories :: HasCallStack => [Path] -> String -> m [Path]

  -- | @since 0.1
  findFile :: HasCallStack => [Path] -> String -> m (Maybe Path)

  -- | @since 0.1
  findFiles :: HasCallStack => [Path] -> String -> m [Path]

  -- | @since 0.1
  findFileWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> m (Maybe Path)

  -- | @since 0.1
  findFilesWith :: HasCallStack => (Path -> m Bool) -> [Path] -> String -> m [Path]

  -- | @since 0.1
  pathIsSymbolicLink :: HasCallStack => Path -> m Bool

  -- | @since 0.1
  getSymbolicLinkTarget :: HasCallStack => Path -> m Path

  -- | @since 0.1
  getPermissions :: HasCallStack => Path -> m Permissions

  -- | @since 0.1
  getAccessTime :: HasCallStack => Path -> m UTCTime

  -- | @since 0.1
  getModificationTime :: HasCallStack => Path -> m UTCTime

instance MonadPathReader IO where
  listDirectory = addCallStack . Dir.listDirectory
  getDirectoryContents = addCallStack . Dir.getDirectoryContents
  getCurrentDirectory = addCallStack Dir.getCurrentDirectory
  getHomeDirectory = addCallStack Dir.getHomeDirectory
  getXdgDirectory d = addCallStack . Dir.getXdgDirectory d
  getXdgDirectoryList = addCallStack . Dir.getXdgDirectoryList
  getAppUserDataDirectory = addCallStack . Dir.getAppUserDataDirectory
  getUserDocumentsDirectory = addCallStack Dir.getUserDocumentsDirectory
  getTemporaryDirectory = addCallStack Dir.getTemporaryDirectory
  getFileSize = addCallStack . fmap fromIntegral . Dir.getFileSize
  canonicalizePath = addCallStack . Dir.canonicalizePath
  makeAbsolute = addCallStack . Dir.makeAbsolute
  makeRelativeToCurrentDirectory = addCallStack . Dir.makeRelativeToCurrentDirectory
  doesPathExist = addCallStack . Dir.doesPathExist
  doesFileExist = addCallStack . Dir.doesFileExist
  doesDirectoryExist = addCallStack . Dir.doesDirectoryExist
  findExecutable = addCallStack . Dir.findExecutable
  findExecutables = addCallStack . Dir.findExecutables
  findExecutablesInDirectories ps = addCallStack . Dir.findExecutablesInDirectories ps
  findFile ps = addCallStack . Dir.findFile ps
  findFiles ps = addCallStack . Dir.findFiles ps
  findFileWith f ps = addCallStack . Dir.findFileWith f ps
  findFilesWith f ps = addCallStack . Dir.findFilesWith f ps
  pathIsSymbolicLink = addCallStack . Dir.pathIsSymbolicLink
  getSymbolicLinkTarget = addCallStack . Dir.getSymbolicLinkTarget
  getPermissions = addCallStack . Dir.getPermissions
  getAccessTime = addCallStack . Dir.getAccessTime
  getModificationTime = addCallStack . Dir.getModificationTime

instance MonadPathReader m => MonadPathReader (ReaderT env m) where
  listDirectory = lift . listDirectory
  getDirectoryContents = lift . getDirectoryContents
  getCurrentDirectory = lift getCurrentDirectory
  getHomeDirectory = lift getHomeDirectory
  getXdgDirectory d = lift . getXdgDirectory d
  getXdgDirectoryList = lift . getXdgDirectoryList
  getAppUserDataDirectory = lift . getAppUserDataDirectory
  getUserDocumentsDirectory = lift getUserDocumentsDirectory
  getTemporaryDirectory = lift getTemporaryDirectory
  getFileSize = lift . getFileSize
  canonicalizePath = lift . canonicalizePath
  makeAbsolute = lift . makeAbsolute
  makeRelativeToCurrentDirectory = lift . makeRelativeToCurrentDirectory
  doesPathExist = lift . doesPathExist
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  findExecutable = lift . findExecutable
  findExecutables = lift . findExecutables
  findExecutablesInDirectories ps = lift . findExecutablesInDirectories ps
  findFile ps = lift . findFile ps
  findFiles ps = lift . findFiles ps
  findFileWith action ps fileName =
    ask >>= lift . \e -> findFileWith ((`runReaderT` e) . action) ps fileName
  findFilesWith action ps fileName =
    ask >>= lift . \e -> findFilesWith ((`runReaderT` e) . action) ps fileName
  pathIsSymbolicLink = lift . pathIsSymbolicLink
  getSymbolicLinkTarget = lift . getSymbolicLinkTarget
  getPermissions = lift . getPermissions
  getAccessTime = lift . getAccessTime
  getModificationTime = lift . getModificationTime

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgConfig = getXdgDirectory XdgConfig
