{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides the MonadPathReader effect.
--
-- @since 0.1
module Effects.FileSystem.PathReader
  ( -- * Effect
    MonadPathReader (..),
    Path,

    -- ** Functions
    findFile,
    findFiles,
    

    -- * Xdg Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
#if MIN_VERSION_directory(1,3,7)
    getXdgState,
#endif

    -- * Misc
    listDirectoryRecursive,

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
import Effects.Exception (addCS)
import Effects.FileSystem.Path (Path, (</>))
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

{- ORMOLU_ENABLE -}

-- REVIEW: Can we reduce the class size by implementing some of these
-- functions in terms of others?

-- | Represents file-system reader effects.
--
-- @since 0.1
class Monad m => MonadPathReader m where
  -- | Lifted 'Dir.listDirectory'.
  --
  -- @since 0.1
  listDirectory :: HasCallStack => Path -> m [Path]

  -- | Lifted 'Dir.getDirectoryContents'.
  --
  -- @since 0.1
  getDirectoryContents :: HasCallStack => Path -> m [Path]

  -- | Lifted 'Dir.getCurrentDirectory'.
  --
  -- @since 0.1
  getCurrentDirectory :: HasCallStack => m Path

  -- | Lifted 'Dir.getHomeDirectory'.
  --
  -- @since 0.1
  getHomeDirectory :: HasCallStack => m Path

  -- | Lifted 'Dir.getXdgDirectory'.
  --
  -- @since 0.1
  getXdgDirectory :: HasCallStack => XdgDirectory -> Path -> m Path

  -- | Lifted 'Dir.getXdgDirectoryList'.
  --
  -- @since 0.1
  getXdgDirectoryList :: HasCallStack => XdgDirectoryList -> m [Path]

  -- | Lifted 'Dir.getAppUserDataDirectory'.
  --
  -- @since 0.1
  getAppUserDataDirectory :: HasCallStack => Path -> m Path

  -- | Lifted 'Dir.getUserDocumentsDirectory'.
  --
  -- @since 0.1
  getUserDocumentsDirectory :: HasCallStack => m Path

  -- | Lifted 'Dir.getTemporaryDirectory'.
  --
  -- @since 0.1
  getTemporaryDirectory :: HasCallStack => m Path

  -- | Lifted 'Dir.getFileSize'.
  --
  -- @since 0.1
  getFileSize :: HasCallStack => Path -> m Integer

  -- | Lifted 'Dir.canonicalizePath'.
  --
  -- @since 0.1
  canonicalizePath :: HasCallStack => Path -> m Path

  -- | Lifted 'Dir.makeAbsolute'.
  --
  -- @since 0.1
  makeAbsolute :: HasCallStack => Path -> m Path

  -- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
  --
  -- @since 0.1
  makeRelativeToCurrentDirectory :: HasCallStack => Path -> m Path

  -- | Lifted 'Dir.doesPathExist'.
  --
  -- @since 0.1
  doesPathExist :: HasCallStack => Path -> m Bool

  -- | Lifted 'Dir.doesFileExist'.
  --
  -- @since 0.1
  doesFileExist :: HasCallStack => Path -> m Bool

  -- | Lifted 'Dir.doesDirectoryExist'.
  --
  -- @since 0.1
  doesDirectoryExist :: HasCallStack => Path -> m Bool

  -- | Lifted 'Dir.findExecutable'.
  --
  -- @since 0.1
  findExecutable :: HasCallStack => Path -> m (Maybe Path)

  -- | Lifted 'Dir.findExecutables'.
  --
  -- @since 0.1
  findExecutables :: HasCallStack => Path -> m [Path]

  -- | Lifted 'Dir.findExecutablesInDirectories'.
  --
  -- @since 0.1
  findExecutablesInDirectories :: HasCallStack => [Path] -> Path -> m [Path]

  -- | Lifted 'Dir.findFileWith'.
  --
  -- @since 0.1
  findFileWith :: HasCallStack => (Path -> m Bool) -> [Path] -> Path -> m (Maybe Path)

  -- | Lifted 'Dir.findFilesWith'.
  --
  -- @since 0.1
  findFilesWith :: HasCallStack => (Path -> m Bool) -> [Path] -> Path -> m [Path]

  -- | Lifted 'Dir.pathIsSymbolicLink'.
  --
  -- @since 0.1
  pathIsSymbolicLink :: HasCallStack => Path -> m Bool

  -- | Lifted 'Dir.getSymbolicLinkTarget'.
  --
  -- @since 0.1
  getSymbolicLinkTarget :: HasCallStack => Path -> m Path

  -- | Lifted 'Dir.getPermissions'.
  --
  -- @since 0.1
  getPermissions :: HasCallStack => Path -> m Permissions

  -- | Lifted 'Dir.getAccessTime'.
  --
  -- @since 0.1
  getAccessTime :: HasCallStack => Path -> m UTCTime

  -- | Lifted 'Dir.getModificationTime'.
  --
  -- @since 0.1
  getModificationTime :: HasCallStack => Path -> m UTCTime

instance MonadPathReader IO where
  listDirectory = addCS . Dir.listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = addCS . Dir.getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = addCS Dir.getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = addCS Dir.getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = addCS . Dir.getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = addCS . Dir.getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = addCS . Dir.getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = addCS Dir.getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = addCS Dir.getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = addCS . Dir.getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = addCS . Dir.canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = addCS . Dir.makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = addCS . Dir.makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = addCS . Dir.doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = addCS . Dir.doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = addCS . Dir.doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = addCS . Dir.findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = addCS . Dir.findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = addCS . Dir.findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFileWith f ps = addCS . Dir.findFileWith f ps
  {-# INLINEABLE findFileWith #-}
  findFilesWith f ps = addCS . Dir.findFilesWith f ps
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = addCS . Dir.pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = addCS . Dir.getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = addCS . Dir.getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = addCS . Dir.getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = addCS . Dir.getModificationTime
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

-- | Lifted 'Dir.findFile'.
--
-- @since 0.1
findFile :: (HasCallStack, MonadPathReader m) => [Path] -> Path -> m (Maybe Path)
findFile = findFileWith (\_ -> pure True)
{-# INLINEABLE findFile #-}

-- | Lifted 'Dir.findFiles'.
--
-- @since 0.1
findFiles :: (HasCallStack, MonadPathReader m) => [Path] -> Path -> m [Path]
findFiles = findFilesWith (\_ -> pure True)
{-# INLINEABLE findFiles #-}

-- | Retrieves the Xdg data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgData = getXdgDirectory XdgData
{-# INLINEABLE getXdgData #-}

-- | Retrieves the Xdg config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgConfig = getXdgDirectory XdgConfig
{-# INLINEABLE getXdgConfig #-}

-- | Retrieves the Xdg cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgCache = getXdgDirectory XdgCache
{-# INLINEABLE getXdgCache #-}

#if MIN_VERSION_directory(1,3,7)
-- | Retrieves the Xdg state directory e.g. @~/.local/state@.
--
-- @since 0.1
getXdgState :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgState = getXdgDirectory XdgState
{-# INLINEABLE getXdgState #-}
#endif

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall m.
  ( HasCallStack,
    MonadPathReader m
  ) =>
  -- | Root path.
  Path ->
  -- | (files, directories)
  m ([Path], [Path])
listDirectoryRecursive root = recurseDirs [""]
  where
    recurseDirs :: [Path] -> m ([Path], [Path])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')

splitPaths ::
  forall m.
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Path ->
  Path ->
  [Path] ->
  [Path] ->
  [Path] ->
  m ([Path], [Path])
splitPaths root d = go
  where
    go :: [Path] -> [Path] -> [Path] -> m ([Path], [Path])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps
