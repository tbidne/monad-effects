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
  getFileSize :: HasCallStack => Path -> m Integer

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
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = addCallStack . Dir.getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = addCallStack Dir.getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = addCallStack Dir.getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = addCallStack . Dir.getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = addCallStack . Dir.getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = addCallStack . Dir.getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = addCallStack Dir.getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = addCallStack Dir.getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = addCallStack . Dir.getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = addCallStack . Dir.canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = addCallStack . Dir.makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = addCallStack . Dir.makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = addCallStack . Dir.doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = addCallStack . Dir.doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = addCallStack . Dir.doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = addCallStack . Dir.findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = addCallStack . Dir.findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = addCallStack . Dir.findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFile ps = addCallStack . Dir.findFile ps
  {-# INLINEABLE findFile #-}
  findFiles ps = addCallStack . Dir.findFiles ps
  {-# INLINEABLE findFiles #-}
  findFileWith f ps = addCallStack . Dir.findFileWith f ps
  {-# INLINEABLE findFileWith #-}
  findFilesWith f ps = addCallStack . Dir.findFilesWith f ps
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = addCallStack . Dir.pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = addCallStack . Dir.getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = addCallStack . Dir.getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = addCallStack . Dir.getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = addCallStack . Dir.getModificationTime
  {-# INLINEABLE getModificationTime #-}

instance MonadPathReader m => MonadPathReader (ReaderT env m) where
  listDirectory = lift . listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = lift . getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = lift getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = lift getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = lift . getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = lift . getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = lift . getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = lift getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = lift getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = lift . getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = lift . canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = lift . makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = lift . makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = lift . doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = lift . doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = lift . doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = lift . findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = lift . findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = lift . findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFile ps = lift . findFile ps
  {-# INLINEABLE findFile #-}
  findFiles ps = lift . findFiles ps
  {-# INLINEABLE findFiles #-}
  findFileWith action ps fileName =
    ask >>= lift . \e -> findFileWith ((`runReaderT` e) . action) ps fileName
  {-# INLINEABLE findFileWith #-}
  findFilesWith action ps fileName =
    ask >>= lift . \e -> findFilesWith ((`runReaderT` e) . action) ps fileName
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = lift . pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = lift . getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = lift . getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = lift . getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = lift . getModificationTime
  {-# INLINEABLE getModificationTime #-}

-- | Retrieves the Xdg Config directory.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgConfig = getXdgDirectory XdgConfig
{-# INLINEABLE getXdgConfig #-}
