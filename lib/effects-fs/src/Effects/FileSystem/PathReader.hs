-- | Provides the MonadPathReader effect.
--
-- @since 0.1
module Effects.FileSystem.PathReader
  ( -- * Effect
    MonadPathReader (..),
    OsPath,

    -- ** Functions
    findFile,
    findFiles,

    -- * XDG Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
    getXdgState,

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
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effects.Exception (addCS)
import Effects.FileSystem.Utils (OsPath, (</>))
import GHC.Stack (HasCallStack)
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadPathReader m where
  -- | Lifted 'Dir.listDirectory'.
  --
  -- @since 0.1
  listDirectory :: (HasCallStack) => OsPath -> m [OsPath]

  -- | Lifted 'Dir.getDirectoryContents'.
  --
  -- @since 0.1
  getDirectoryContents :: (HasCallStack) => OsPath -> m [OsPath]

  -- | Lifted 'Dir.getCurrentDirectory'.
  --
  -- @since 0.1
  getCurrentDirectory :: (HasCallStack) => m OsPath

  -- | Lifted 'Dir.getHomeDirectory'.
  --
  -- @since 0.1
  getHomeDirectory :: (HasCallStack) => m OsPath

  -- | Lifted 'Dir.getXdgDirectory'.
  --
  -- @since 0.1
  getXdgDirectory :: (HasCallStack) => XdgDirectory -> OsPath -> m OsPath

  -- | Lifted 'Dir.getXdgDirectoryList'.
  --
  -- @since 0.1
  getXdgDirectoryList :: (HasCallStack) => XdgDirectoryList -> m [OsPath]

  -- | Lifted 'Dir.getAppUserDataDirectory'.
  --
  -- @since 0.1
  getAppUserDataDirectory :: (HasCallStack) => OsPath -> m OsPath

  -- | Lifted 'Dir.getUserDocumentsDirectory'.
  --
  -- @since 0.1
  getUserDocumentsDirectory :: (HasCallStack) => m OsPath

  -- | Lifted 'Dir.getTemporaryDirectory'.
  --
  -- @since 0.1
  getTemporaryDirectory :: (HasCallStack) => m OsPath

  -- | Lifted 'Dir.getFileSize'.
  --
  -- @since 0.1
  getFileSize :: (HasCallStack) => OsPath -> m Integer

  -- | Lifted 'Dir.canonicalizePath'.
  --
  -- @since 0.1
  canonicalizePath :: (HasCallStack) => OsPath -> m OsPath

  -- | Lifted 'Dir.makeAbsolute'.
  --
  -- @since 0.1
  makeAbsolute :: (HasCallStack) => OsPath -> m OsPath

  -- | Lifted 'Dir.makeRelativeToCurrentDirectory'.
  --
  -- @since 0.1
  makeRelativeToCurrentDirectory :: (HasCallStack) => OsPath -> m OsPath

  -- | Lifted 'Dir.doesPathExist'.
  --
  -- @since 0.1
  doesPathExist :: (HasCallStack) => OsPath -> m Bool

  -- | Lifted 'Dir.doesFileExist'.
  --
  -- @since 0.1
  doesFileExist :: (HasCallStack) => OsPath -> m Bool

  -- | Lifted 'Dir.doesDirectoryExist'.
  --
  -- @since 0.1
  doesDirectoryExist :: (HasCallStack) => OsPath -> m Bool

  -- | Lifted 'Dir.findExecutable'.
  --
  -- @since 0.1
  findExecutable :: (HasCallStack) => OsPath -> m (Maybe OsPath)

  -- | Lifted 'Dir.findExecutables'.
  --
  -- @since 0.1
  findExecutables :: (HasCallStack) => OsPath -> m [OsPath]

  -- | Lifted 'Dir.findExecutablesInDirectories'.
  --
  -- @since 0.1
  findExecutablesInDirectories :: (HasCallStack) => [OsPath] -> OsPath -> m [OsPath]

  -- | Lifted 'Dir.findFileWith'.
  --
  -- @since 0.1
  findFileWith :: (HasCallStack) => (OsPath -> m Bool) -> [OsPath] -> OsPath -> m (Maybe OsPath)

  -- | Lifted 'Dir.findFilesWith'.
  --
  -- @since 0.1
  findFilesWith :: (HasCallStack) => (OsPath -> m Bool) -> [OsPath] -> OsPath -> m [OsPath]

  -- | Lifted 'Dir.pathIsSymbolicLink'.
  --
  -- @since 0.1
  pathIsSymbolicLink :: (HasCallStack) => OsPath -> m Bool

  -- | Lifted 'Dir.getSymbolicLinkTarget'.
  --
  -- @since 0.1
  getSymbolicLinkTarget :: (HasCallStack) => OsPath -> m OsPath

  -- | Lifted 'Dir.getPermissions'.
  --
  -- @since 0.1
  getPermissions :: (HasCallStack) => OsPath -> m Permissions

  -- | Lifted 'Dir.getAccessTime'.
  --
  -- @since 0.1
  getAccessTime :: (HasCallStack) => OsPath -> m UTCTime

  -- | Lifted 'Dir.getModificationTime'.
  --
  -- @since 0.1
  getModificationTime :: (HasCallStack) => OsPath -> m UTCTime

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

instance (MonadPathReader m) => MonadPathReader (ReaderT env m) where
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

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile :: (HasCallStack, MonadPathReader m) => [OsPath] -> OsPath -> m (Maybe OsPath)
findFile = findFileWith (\_ -> pure True)
{-# INLINEABLE findFile #-}

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles :: (HasCallStack, MonadPathReader m) => [OsPath] -> OsPath -> m [OsPath]
findFiles = findFilesWith (\_ -> pure True)
{-# INLINEABLE findFiles #-}

-- | Retrieves the XDG data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData :: (HasCallStack, MonadPathReader m) => OsPath -> m OsPath
getXdgData = getXdgDirectory XdgData
{-# INLINEABLE getXdgData #-}

-- | Retrieves the XDG config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadPathReader m) => OsPath -> m OsPath
getXdgConfig = getXdgDirectory XdgConfig
{-# INLINEABLE getXdgConfig #-}

-- | Retrieves the XDG cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache :: (HasCallStack, MonadPathReader m) => OsPath -> m OsPath
getXdgCache = getXdgDirectory XdgCache
{-# INLINEABLE getXdgCache #-}

-- | Retrieves the XDG state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState :: (HasCallStack, MonadPathReader m) => OsPath -> m OsPath
getXdgState = getXdgDirectory XdgState
{-# INLINEABLE getXdgState #-}

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
  OsPath ->
  -- | (files, directories)
  m ([OsPath], [OsPath])
listDirectoryRecursive root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> m ([OsPath], [OsPath])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')
    emptyPath = mempty

splitPaths ::
  forall m.
  ( HasCallStack,
    MonadPathReader m
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  m ([OsPath], [OsPath])
splitPaths root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> m ([OsPath], [OsPath])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps
