{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

    -- * Path Types
    PathType (..),

    -- ** Functions
    displayPathType,
    getPathType,
    isPathType,
    throwIfWrongPathType,

    -- * Tilde expansion
    expandTilde,
    forExpandedTilde,
    onExpandedTilde,

    -- * Misc
    listDirectoryRecursive,
    listDirectoryRecursiveSymbolicLink,
    doesSymbolicLinkExist,
    pathIsSymbolicDirectoryLink,
    pathIsSymbolicFileLink,

    -- * Re-exports
    XdgDirectory (..),
    XdgDirectoryList (..),
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch qualified as Ex
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath
  ( OsPath,
    OsPathOrEmpty (OsPathEmpty, OsPathNonEmpty),
    TildePrefixState
      ( TildePrefixStateNone,
        TildePrefixStateStripped
      ),
    (</>),
  )
import FileSystem.OsPath qualified as OsP
import FileSystem.PathType
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeOther,
        PathTypeSymbolicLink
      ),
    displayPathType,
  )
import GHC.IO.Exception (IOErrorType (InappropriateType))
import GHC.Stack (HasCallStack)
import System.Directory
  ( Permissions,
    XdgDirectory (XdgCache, XdgConfig, XdgData, XdgState),
    XdgDirectoryList (XdgConfigDirs, XdgDataDirs),
  )
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as IO.Error

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
  listDirectory = Dir.listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = Dir.getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = Dir.getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = Dir.getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory = Dir.getXdgDirectory
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = Dir.getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = Dir.getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = Dir.getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = Dir.getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = Dir.getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = Dir.canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = Dir.makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = Dir.makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = Dir.doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = Dir.doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = Dir.doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = Dir.findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = Dir.findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories = Dir.findExecutablesInDirectories
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFileWith = Dir.findFileWith
  {-# INLINEABLE findFileWith #-}
  findFilesWith = Dir.findFilesWith
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = Dir.pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = Dir.getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = Dir.getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = Dir.getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = Dir.getModificationTime
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
{-# INLINEABLE listDirectoryRecursive #-}

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
{-# INLINEABLE splitPaths #-}

-- | Like 'listDirectoryRecursive' except symbolic links are not traversed
-- i.e. they are returned separately.
--
-- @since 0.1
listDirectoryRecursiveSymbolicLink ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  -- | Root path.
  OsPath ->
  -- | (files, directories, symbolic links)
  m ([OsPath], [OsPath], [OsPath])
listDirectoryRecursiveSymbolicLink root = recurseDirs [emptyPath]
  where
    recurseDirs :: [OsPath] -> m ([OsPath], [OsPath], [OsPath])
    recurseDirs [] = pure ([], [], [])
    recurseDirs (d : ds) = do
      (files, dirs, symlinks) <-
        splitPathsSymboliclink root d [] [] [] =<< listDirectory (root </> d)
      (files', dirs', symlinks') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs', symlinks ++ symlinks')
    emptyPath = mempty
{-# INLINEABLE listDirectoryRecursiveSymbolicLink #-}

splitPathsSymboliclink ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  OsPath ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  [OsPath] ->
  m ([OsPath], [OsPath], [OsPath])
splitPathsSymboliclink root d = go
  where
    go :: [OsPath] -> [OsPath] -> [OsPath] -> [OsPath] -> m ([OsPath], [OsPath], [OsPath])
    go files dirs symlinks [] = pure (reverse files, reverse dirs, symlinks)
    go files dirs symlinks (p : ps) = do
      let dirEntry = d </> p
          fullPath = root </> dirEntry

      isSymlink <- doesSymbolicLinkExist fullPath
      if isSymlink
        then go files dirs (dirEntry : symlinks) ps
        else do
          isDir <- doesDirectoryExist fullPath
          if isDir
            then go files (dirEntry : dirs) symlinks ps
            else go (dirEntry : files) dirs symlinks ps
{-# INLINEABLE splitPathsSymboliclink #-}

{- ORMOLU_DISABLE -}

-- | Returns true if the path is a symbolic link. Does not traverse the link.
--
-- @since 0.1
doesSymbolicLinkExist ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m Bool
doesSymbolicLinkExist p =
  -- pathIsSymbolicLink throws an exception if the path does not exist,
  -- so we need to handle this. Note that the obvious alternative, prefacing
  -- the call with doesPathExist does not work, as that operates on the link
  -- target. doesFileExist also behaves this way.
  pathIsSymbolicLink p `Ex.catchIOError` \_ -> pure False
{-# INLINEABLE doesSymbolicLinkExist #-}

{- ORMOLU_ENABLE -}

-- | Like 'pathIsSymbolicDirectoryLink' but for files.
--
-- @since 0.1
pathIsSymbolicFileLink ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  OsPath ->
  m Bool
pathIsSymbolicFileLink = getSymbolicLinkTarget >=> doesFileExist
{-# INLINEABLE pathIsSymbolicFileLink #-}

-- | Returns true if @p@ is a symbolic link and it points to an extant
-- directory. Throws an exception if the path is not a symbolic link or the
-- target does not exist.
--
-- This function and 'pathIsSymbolicFileLink' are intended to distinguish file
-- and directory links on Windows. This matters for knowing when to use:
--
--     - @createFileLink@ vs. @createDirectoryLink@
--     - @removeFile@ vs. @removeDirectoryLink@
--
-- Suppose we want to copy an arbitrary path @p@. We first determine that
-- @p@ is a symlink via 'doesSymbolicLinkExist'. If
-- 'pathIsSymbolicDirectoryLink' returns true then we know we should use
-- "Effects.FileSystem.PathWriter"'s @createDirectoryLink@. Otherwise we can
-- fall back to @createFileLink@.
--
-- Because this relies on the symlink's target, this is best effort, and it is
-- possible 'pathIsSymbolicDirectoryLink' and 'pathIsSymbolicFileLink' both
-- return false.
--
-- Note that Posix makes no distinction between file and directory symbolic
-- links. Thus if your system only has to work on Posix, you probably don't
-- need this function.
--
-- @since 0.1
pathIsSymbolicDirectoryLink ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  OsPath ->
  m Bool
pathIsSymbolicDirectoryLink = getSymbolicLinkTarget >=> doesDirectoryExist
{-# INLINEABLE pathIsSymbolicDirectoryLink #-}

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- For a faster version in terms of Posix(Compat), see effects-unix(-compat).
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  -- | The location for the thrown exception (e.g. function name)
  String ->
  -- | Expected path type
  PathType ->
  -- | Path
  OsPath ->
  m ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path to have type ",
            displayPathType expected,
            ", but detected ",
            displayPathType actual
          ]

  unless (expected == actual) $
    FS.IO.throwPathIOError
      path
      location
      InappropriateType
      err
{-# INLINEABLE throwIfWrongPathType #-}

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- For a faster version in terms of Posix(Compat), see effects-unix(-compat).
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  -- | Expected path type.
  PathType ->
  -- Path.
  OsPath ->
  m Bool
isPathType expected = fmap (== expected) . getPathType
{-# INLINEABLE isPathType #-}

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- For a faster version in terms of PosixCompat, see effects-unix-compat.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  OsPath ->
  m PathType
getPathType path = do
  -- This needs to be first as does(Directory|File|Path)Exist acts on the target.
  symlinkExists <- doesSymbolicLinkExist path
  if symlinkExists
    then pure PathTypeSymbolicLink
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          fileExists <- doesFileExist path
          if fileExists
            then pure PathTypeFile
            else do
              pathExists <- doesPathExist path
              if pathExists
                then pure PathTypeOther
                else
                  FS.IO.throwPathIOError
                    path
                    "getPathType"
                    IO.Error.doesNotExistErrorType
                    "path does not exist"
{-# INLINEABLE getPathType #-}

-- | 'onExpandedTilde' that simply returns the expanded path.
--
-- @since 0.1
expandTilde ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Path to potentially expand.
  OsPath ->
  m OsPath
expandTilde = onExpandedTilde pure
{-# INLINEABLE expandTilde #-}

-- | Flipped 'onExpandedTilde'.
--
-- @since 0.1
forExpandedTilde ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Path to potentially expand.
  OsPath ->
  -- | Action to run on the expanded path.
  (OsPath -> m a) ->
  m a
forExpandedTilde = flip onExpandedTilde
{-# INLINEABLE forExpandedTilde #-}

-- | Expands "tilde prefix(es)" (~) with the home directory, running the
-- action on the result.
--
-- - @"~/..."@
-- - @"~"@
-- - @"~\\..."@ (windows only)
--
-- If the path contains no tildes, it is handled normally.
--
-- @since 0.1
onExpandedTilde ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  -- | Action to run on the expanded path.
  (OsPath -> m a) ->
  -- | Path to potentially expand.
  OsPath ->
  m a
onExpandedTilde onPath =
  OsP.toTildePrefixState >>> \case
    TildePrefixStateNone p -> onPath p
    TildePrefixStateStripped pne ->
      getHomeDirectory >>= \d -> case pne of
        OsPathEmpty -> onPath d
        OsPathNonEmpty p -> onPath $ d </> p
{-# INLINEABLE onExpandedTilde #-}
