{-# LANGUAGE UndecidableInstances #-}

-- | Provides the MonadPathWriter effect.
--
-- @since 0.1
module Effects.FileSystem.PathWriter
  ( -- * Effect
    MonadPathWriter (..),
    OsPath,

    -- * Copying

    -- ** Config
    CopyDirConfig (..),
    Overwrite (..),
    TargetName (..),
    defaultCopyDirConfig,

    -- ** Functions
    copyDirectoryRecursive,
    copyDirectoryRecursiveConfig,
    copySymbolicLink,

    -- ** Optics
    _OverwriteNone,
    _OverwriteDirectories,
    _OverwriteAll,
    _TargetNameSrc,
    _TargetNameLiteral,
    _TargetNameDest,

    -- * Removing
    -- $if-exists
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- ** Symbolic Links
    removeSymbolicLink,
    removeSymbolicLinkIfExists,

    -- * Reexports
    IOException,
    Permissions (..),
    UTCTime (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (IOException)
import Control.Exception.Utils (onSyncException)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, mask_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Foldable (for_, traverse_)
import Data.Time (UTCTime (UTCTime, utctDay, utctDayTime))
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( doesDirectoryExist,
        doesFileExist,
        doesPathExist,
        getSymbolicLinkTarget
      ),
    PathType (PathTypeDirectory, PathTypeSymbolicLink),
    doesSymbolicLinkExist,
    listDirectoryRecursiveSymbolicLink,
    pathIsSymbolicDirectoryLink,
  )
import Effects.FileSystem.PathReader qualified as PR
import Effects.IORef
  ( MonadIORef (modifyIORef', newIORef, readIORef),
  )
import FileSystem.IO qualified as FS.IO
import FileSystem.OsPath (OsPath, (</>))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics.Core
  ( A_Lens,
    LabelOptic (labelOptic),
    Prism',
    lensVL,
    prism,
    (^.),
  )
import System.Directory (Permissions)
import System.Directory.OsPath qualified as Dir
import System.IO.Error qualified as Error
import System.OsPath qualified as FP

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadPathWriter m where
  -- | Lifted 'Dir.createDirectory'.
  --
  -- @since 0.1
  createDirectory :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.createDirectoryIfMissing'.
  --
  -- @since 0.1
  createDirectoryIfMissing ::
    (HasCallStack) =>
    -- | Create its parents too?
    Bool ->
    -- | The path to the directory you want to make
    OsPath ->
    m ()

  -- | Lifted 'Dir.removeDirectory'.
  --
  -- @since 0.1
  removeDirectory :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.removeDirectoryRecursive'.
  --
  -- @since 0.1
  removeDirectoryRecursive :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.removePathForcibly'.
  --
  -- @since 0.1
  removePathForcibly :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.renameDirectory'.
  --
  -- @since 0.1
  renameDirectory :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.setCurrentDirectory'.
  --
  -- @since 0.1
  setCurrentDirectory :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.withCurrentDirectory'.
  --
  -- @since 0.1
  withCurrentDirectory :: (HasCallStack) => OsPath -> m a -> m a

  -- | Lifted 'Dir.removeFile'.
  --
  -- @since 0.1
  removeFile :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.renameFile'.
  --
  -- @since 0.1
  renameFile :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.renamePath'.
  --
  -- @since 0.1
  renamePath ::
    (HasCallStack) =>
    -- | Old path
    OsPath ->
    -- | New path
    OsPath ->
    m ()

  -- | Lifted 'Dir.copyFile'.
  --
  -- @since 0.1
  copyFile ::
    (HasCallStack) =>
    -- | Source filename
    OsPath ->
    -- | Destination filename
    OsPath ->
    m ()

  -- | Lifted 'Dir.copyFileWithMetadata'.
  --
  -- @since 0.1
  copyFileWithMetadata ::
    (HasCallStack) =>
    -- | Source file
    OsPath ->
    -- | Destination file
    OsPath ->
    m ()

  -- | Lifted 'Dir.createFileLink'.
  --
  -- @since 0.1
  createFileLink ::
    (HasCallStack) =>
    -- | path to the target file
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Lifted 'Dir.createDirectoryLink'.
  --
  -- @since 0.1
  createDirectoryLink ::
    (HasCallStack) =>
    -- | path to the target directory
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Lifted 'Dir.removeDirectoryLink'.
  --
  -- @since 0.1
  removeDirectoryLink :: (HasCallStack) => OsPath -> m ()

  -- | Lifted 'Dir.setPermissions'.
  --
  -- @since 0.1
  setPermissions :: (HasCallStack) => OsPath -> Permissions -> m ()

  -- | Lifted 'Dir.copyPermissions'.
  --
  -- @since 0.1
  copyPermissions :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Lifted 'Dir.setAccessTime'.
  --
  -- @since 0.1
  setAccessTime :: (HasCallStack) => OsPath -> UTCTime -> m ()

  -- | Lifted 'Dir.setModificationTime'.
  --
  -- @since 0.1
  setModificationTime :: (HasCallStack) => OsPath -> UTCTime -> m ()

-- | @since 0.1
instance MonadPathWriter IO where
  createDirectory = Dir.createDirectory
  {-# INLINEABLE createDirectory #-}
  createDirectoryIfMissing = Dir.createDirectoryIfMissing
  {-# INLINEABLE createDirectoryIfMissing #-}
  removeDirectory = Dir.removeDirectory
  {-# INLINEABLE removeDirectory #-}
  removeDirectoryRecursive = Dir.removeDirectoryRecursive
  {-# INLINEABLE removeDirectoryRecursive #-}
  removePathForcibly = Dir.removePathForcibly
  {-# INLINEABLE removePathForcibly #-}
  renameDirectory = Dir.renameDirectory
  {-# INLINEABLE renameDirectory #-}
  setCurrentDirectory = Dir.setCurrentDirectory
  {-# INLINEABLE setCurrentDirectory #-}
  withCurrentDirectory = Dir.withCurrentDirectory
  {-# INLINEABLE withCurrentDirectory #-}
  removeFile = Dir.removeFile
  {-# INLINEABLE removeFile #-}
  renameFile = Dir.renameFile
  {-# INLINEABLE renameFile #-}
  renamePath = Dir.renamePath
  {-# INLINEABLE renamePath #-}
  copyFile = Dir.copyFile
  {-# INLINEABLE copyFile #-}
  copyFileWithMetadata = Dir.copyFileWithMetadata
  {-# INLINEABLE copyFileWithMetadata #-}
  createFileLink = Dir.createFileLink
  {-# INLINEABLE createFileLink #-}
  createDirectoryLink = Dir.createDirectoryLink
  {-# INLINEABLE createDirectoryLink #-}
  removeDirectoryLink = Dir.removeDirectoryLink
  {-# INLINEABLE removeDirectoryLink #-}
  setPermissions = Dir.setPermissions
  {-# INLINEABLE setPermissions #-}
  copyPermissions = Dir.copyPermissions
  {-# INLINEABLE copyPermissions #-}
  setAccessTime = Dir.setAccessTime
  {-# INLINEABLE setAccessTime #-}
  setModificationTime = Dir.setModificationTime
  {-# INLINEABLE setModificationTime #-}

-- | @since 0.1
instance (MonadPathWriter m) => MonadPathWriter (ReaderT env m) where
  createDirectory = lift . createDirectory
  {-# INLINEABLE createDirectory #-}
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
  {-# INLINEABLE createDirectoryIfMissing #-}
  removeDirectory = lift . removeDirectory
  {-# INLINEABLE removeDirectory #-}
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  {-# INLINEABLE removeDirectoryRecursive #-}
  removePathForcibly = lift . removePathForcibly
  {-# INLINEABLE removePathForcibly #-}
  renameDirectory p = lift . renameDirectory p
  {-# INLINEABLE renameDirectory #-}
  setCurrentDirectory = lift . setCurrentDirectory
  {-# INLINEABLE setCurrentDirectory #-}
  withCurrentDirectory p action =
    ask >>= lift . \e -> withCurrentDirectory p (runReaderT action e)
  {-# INLINEABLE withCurrentDirectory #-}
  removeFile = lift . removeFile
  {-# INLINEABLE removeFile #-}
  renameFile p = lift . renameFile p
  {-# INLINEABLE renameFile #-}
  renamePath p = lift . renamePath p
  {-# INLINEABLE renamePath #-}
  copyFile p = lift . copyFile p
  {-# INLINEABLE copyFile #-}
  copyFileWithMetadata p = lift . copyFileWithMetadata p
  {-# INLINEABLE copyFileWithMetadata #-}
  createFileLink p = lift . createFileLink p
  {-# INLINEABLE createFileLink #-}
  createDirectoryLink p = lift . createDirectoryLink p
  {-# INLINEABLE createDirectoryLink #-}
  removeDirectoryLink = lift . removeDirectoryLink
  {-# INLINEABLE removeDirectoryLink #-}
  setPermissions p = lift . setPermissions p
  {-# INLINEABLE setPermissions #-}
  copyPermissions p = lift . copyPermissions p
  {-# INLINEABLE copyPermissions #-}
  setAccessTime p = lift . setAccessTime p
  {-# INLINEABLE setAccessTime #-}
  setModificationTime p = lift . setModificationTime p
  {-# INLINEABLE setModificationTime #-}

-- | Determines file/directory overwrite behavior.
--
-- @since 0.1
data Overwrite
  = -- | No overwriting allowed.
    --
    -- @since 0.1
    OverwriteNone
  | -- | Allow overwriting directories.
    --
    -- @since 0.1
    OverwriteDirectories
  | -- | Allow overwriting the target directory and all subpaths.
    --
    -- @since 0.1
    OverwriteAll
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_OverwriteNone :: Prism' Overwrite ()
_OverwriteNone =
  prism
    (const OverwriteNone)
    ( \case
        OverwriteNone -> Right ()
        x -> Left x
    )
{-# INLINE _OverwriteNone #-}

-- | @since 0.1
_OverwriteDirectories :: Prism' Overwrite ()
_OverwriteDirectories =
  prism
    (const OverwriteDirectories)
    ( \case
        OverwriteDirectories -> Right ()
        x -> Left x
    )
{-# INLINE _OverwriteDirectories #-}

-- | @since 0.1
_OverwriteAll :: Prism' Overwrite ()
_OverwriteAll =
  prism
    (const OverwriteAll)
    ( \case
        OverwriteAll -> Right ()
        x -> Left x
    )
{-# INLINE _OverwriteAll #-}

-- | Determines how to name the target.
--
-- @since 0.1
data TargetName
  = -- | Uses the src dir as the dest name i.e. @dest/\<src\>@.
    --
    -- @since 0.1
    TargetNameSrc
  | -- | Uses the given literal as the dest name i.e. @dest/\<targetName\>@.
    --
    -- @since 0.1
    TargetNameLiteral !OsPath
  | -- | Uses dest itself as the target i.e. @dest/@ (top-level copy).
    --
    -- @since 0.1
    TargetNameDest
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_TargetNameSrc :: Prism' TargetName ()
_TargetNameSrc =
  prism
    (const TargetNameSrc)
    ( \case
        TargetNameSrc -> Right ()
        x -> Left x
    )
{-# INLINE _TargetNameSrc #-}

-- | @since 0.1
_TargetNameLiteral :: Prism' TargetName OsPath
_TargetNameLiteral =
  prism
    TargetNameLiteral
    ( \case
        TargetNameLiteral p -> Right p
        x -> Left x
    )
{-# INLINE _TargetNameLiteral #-}

-- | @since 0.1
_TargetNameDest :: Prism' TargetName ()
_TargetNameDest =
  prism
    (const TargetNameDest)
    ( \case
        TargetNameDest -> Right ()
        x -> Left x
    )
{-# INLINE _TargetNameDest #-}

-- | Directory copying config.
--
-- @since 0.1
data CopyDirConfig = MkCopyDirConfig
  { -- | Overwrite behavior.
    --
    -- @since 0.1
    overwrite :: !Overwrite,
    -- | TargetName behavior.
    --
    -- @since 0.1
    targetName :: !TargetName
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Overwrite, b ~ Overwrite) =>
  LabelOptic "overwrite" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig a1 a2) ->
    fmap (\b -> MkCopyDirConfig b a2) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TargetName, b ~ TargetName) =>
  LabelOptic "targetName" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig a1 a2) ->
    fmap (MkCopyDirConfig a1) (f a2)
  {-# INLINE labelOptic #-}

-- | Default config for copying directories.
--
-- >>> defaultCopyDirConfig
-- MkCopyDirConfig {overwrite = OverwriteNone, destName = TargetNameSrc}
--
-- @since 0.1
defaultCopyDirConfig :: CopyDirConfig
defaultCopyDirConfig = MkCopyDirConfig OverwriteNone TargetNameSrc

-- | 'copyDirectoryRecursiveConfig' with 'defaultCopyDirConfig'.
--
-- @since 0.1
copyDirectoryRecursive ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryRecursive = copyDirectoryRecursiveConfig defaultCopyDirConfig

-- | @copyDirectoryRecursiveConfig cfg src dest@ copies the @src@ and its
-- contents into @dest@ e.g.
--
-- @
-- copyDirectoryRecursiveConfig cfg "path\/to\/foo" "path\/to\/bar"
-- @
--
-- will create @path\/to\/bar\/foo@, @path\/to\/bar\/\<target\>@, or copy
-- @foo@'s contents directly into @bar@, depending on the value of
-- 'targetName'.
--
-- The atomicity semantics are as follows:
--
-- * 'OverwriteNone': If an error is encountered, we roll back the successful
--    writes by deleting the entire @dest\/\<target\>@.
-- * 'OverwriteDirectories': If an error is encountered, we attempt to delete
--   all successfully written paths/directories. Because these deletes are
--   performed sequentially, we cannot guarantee all are removed before the
--   process is interrupted.
-- * 'OverwriteAll': Same as 'OverwriteDirectories', except paths that were
--   overwritten are not restored. That is, if a path @dest\/\<src\>\/p@ is
--   overwritten and an error later encountered, @p@ is not restored.
--
-- __Throws:__
--
-- * 'Control.Exception.IOException': if @dest@ does not exist or one of:
--
--     * 'OverwriteNone' and @dest/\<src\>@ exists.
--     * 'OverwriteDirectories' and some @dest/\<target\>\/p@ would be
--        overwritten.
--
-- @since 0.1
copyDirectoryRecursiveConfig ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Config
  CopyDirConfig ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryRecursiveConfig config src destRoot = do
  PR.throwIfWrongPathType "copyDirectoryRecursiveConfig" PathTypeDirectory src
  PR.throwIfWrongPathType "copyDirectoryRecursiveConfig" PathTypeDirectory destRoot

  let dest = case config ^. #targetName of
        -- Use source directory's name
        TargetNameSrc ->
          let -- Previously we used takeBaseName, but this caused a bug
              -- where e.g. dir-1.0.0 -> dir-1.0 (i.e. the last dot was treated
              -- as an extension, that takeBaseName removes).
              --
              -- splitFileName seems to do what we want e.g.
              --
              -- (/path/to/, dir-1.0.0) === splitFileName /path/to/dir-1.0.0
              --
              -- Note that dropTrailingPathSeparator needs to be used first
              -- to ensure correctness.
              --
              -- This also caused a bug where hidden directories were copied
              -- incorrectly.
              (_, name) = FP.splitFileName (FP.dropTrailingPathSeparator src)
           in destRoot </> name
        -- Use the give name
        TargetNameLiteral p -> destRoot </> p
        -- Use dest itself (i.e. top-level copy)
        TargetNameDest -> destRoot

  case config ^. #overwrite of
    OverwriteNone -> copyDirectoryNoOverwrite src dest
    OverwriteDirectories -> copyDirectoryOverwrite False src dest
    OverwriteAll -> copyDirectoryOverwrite True src dest

copyDirectoryOverwrite ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Overwrite files
  Bool ->
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryOverwrite overwriteFiles src dest = do
  -- NOTE: The logic here merits explanation. The idea is if we encounter
  -- any errors while copying, we want to "roll back" any successful copies
  -- i.e. copying should try to be atomic.
  --
  -- In copyDirectory this is simple; we can assume the dest/\<src\> does not
  -- exist (otherwise throwing an exception), so the logic is:
  --
  -- 1. Copying: use (createDirectoryIfMissing True) to create the
  --   necessary parent dirs automatically.
  -- 2. Cleanup: If anything goes wrong, delete the entire dest/\<src\>.
  --
  -- For copyDirectoryOverwrite, however, the dest/\<src\> might already exist,
  -- making our job harder. In particular:
  --
  -- 1. Copying:
  --      - Create the parent directories sequentially. We store the
  --        created paths in an IORef.
  --      - Copy the files over, saving the copied paths to another IORef.
  -- 2. Cleanup:
  --      - If anything goes wrong, we cannot simply delete the dest/\<src\>
  --        because it might have already existed. We iterate through our
  --        IORefs, deleting the paths.

  copiedFilesRef <- newIORef []
  createdDirsRef <- newIORef []
  copiedSymlinksRef <- newIORef []

  destExists <- doesDirectoryExist dest

  let checkFileOverwrites =
        if not overwriteFiles
          then \f -> do
            exists <- doesFileExist f
            when exists $
              FS.IO.throwPathIOError
                f
                "copyDirectoryOverwrite"
                Error.alreadyExistsErrorType
                "Attempted file overwrite when CopyDirConfig.overwriteFiles is false"
          else const (pure ())

      checkSymlinkOverwrites =
        if not overwriteFiles
          then \f -> do
            exists <- doesSymbolicLinkExist f
            when exists $
              FS.IO.throwPathIOError
                f
                "copyDirectoryOverwrite"
                Error.alreadyExistsErrorType
                "Attempted symlink overwrite when CopyDirConfig.overwriteFiles is false"
          else const (pure ())

      copyFiles = do
        (subFiles, subDirs, symlinks) <- listDirectoryRecursiveSymbolicLink src

        -- Create dest if it does not exist. Do not need to save dir
        -- in createdDirsRef IORef as it will be correctly deleted by
        -- removeDirectoryRecursive if necessary.
        unless destExists $ createDirectory dest

        -- create the parent directories
        for_ subDirs $ \d -> do
          let d' = dest </> d
          dExists <- doesDirectoryExist d'
          unless dExists $ do
            createDirectoryIfMissing False d'
            modifyIORef' createdDirsRef (d' :)

        -- copy files
        for_ subFiles $ \f -> do
          let f' = dest </> f
          checkFileOverwrites f'
          copyFileWithMetadata (src </> f) f'
          modifyIORef' copiedFilesRef (f' :)

        -- copy symlinks
        for_ symlinks $ \s -> do
          let s' = dest </> s
          checkSymlinkOverwrites s'
          copySymbolicLink (src </> s) s'
          modifyIORef' copiedSymlinksRef (s' :)

      cleanup =
        if destExists
          then do
            -- manually delete files and dirs
            readIORef copiedFilesRef >>= traverse_ removeFile
            readIORef createdDirsRef >>= traverse_ removeDirectory
            readIORef copiedSymlinksRef >>= traverse_ removeSymbolicLink
          else removeDirectoryRecursive dest

  copyFiles `onSyncException` mask_ cleanup

copyDirectoryNoOverwrite ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Source
  OsPath ->
  -- | Destination
  OsPath ->
  m ()
copyDirectoryNoOverwrite src dest = do
  destExists <- doesDirectoryExist dest
  when destExists $
    FS.IO.throwPathIOError
      dest
      "copyDirectoryNoOverwrite"
      Error.alreadyExistsErrorType
      "Attempted directory overwrite when CopyDirConfig.overwrite is OverwriteNone"

  let copyFiles = do
        (subFiles, subDirs, symlinks) <- listDirectoryRecursiveSymbolicLink src
        createDirectory dest

        -- create intermediate dirs if they do not exist
        traverse_ (createDirectoryIfMissing True . (dest </>)) subDirs

        -- copy files
        for_ subFiles $ \f -> copyFileWithMetadata (src </> f) (dest </> f)

        -- copy symlinks
        for_ symlinks $ \s -> copySymbolicLink (src </> s) (dest </> s)

      -- delete directory
      cleanup = removeDirectoryRecursive dest

  copyFiles `onSyncException` mask_ cleanup

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeFileIfExists = removeIfExists doesFileExist removeFile
{-# INLINEABLE removeFileIfExists #-}

-- | Calls 'removeDirectory' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryIfExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeDirectoryIfExists = removeIfExists doesDirectoryExist removeDirectory
{-# INLINEABLE removeDirectoryIfExists #-}

-- | Calls 'removeDirectoryRecursive' if 'doesDirectoryExist' is 'True'.
--
-- @since 0.1
removeDirectoryRecursiveIfExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeDirectoryRecursiveIfExists =
  removeIfExists doesDirectoryExist removeDirectoryRecursive
{-# INLINEABLE removeDirectoryRecursiveIfExists #-}

-- | Calls 'removePathForcibly' if 'doesPathExist' is 'True'.
--
-- @since 0.1
removePathForciblyIfExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly
{-# INLINEABLE removePathForciblyIfExists #-}

-- | Calls 'removeSymbolicLink' if 'doesSymbolicLinkExist' is 'True'.
--
-- @since 0.1
removeSymbolicLinkIfExists ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeSymbolicLinkIfExists =
  removeIfExists doesSymbolicLinkExist removeSymbolicLink

removeIfExists :: (Monad m) => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
{-# INLINEABLE removeIfExists #-}

-- | Removes a symbolic link. On Windows, attempts to distinguish
-- file and directory links (Posix makes no distinction).
--
-- @since 0.1
removeSymbolicLink ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  OsPath ->
  m ()
removeSymbolicLink p = do
  PR.throwIfWrongPathType "removeSymbolicLink" PathTypeSymbolicLink p

  pathIsSymbolicDirectoryLink p >>= \case
    True -> removeDirectoryLink p
    False -> removeFile p
{-# INLINEABLE removeSymbolicLink #-}

-- | Copies the symbolic link /without/ traversing the link i.e. copy the
-- link itself. Does not throw an exception if the target does exist.
-- Throws an @IOException@ if the path is not a symbolic link.
--
-- __Windows:__ We have to distinguish between file and directory links
-- (Posix makes no such distinction). If the target does not exist or is
-- not considered a directory (e.g. it could also be a link), we fall back
-- to creating a file link.
--
-- @since 0.1
copySymbolicLink ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Source
  OsPath ->
  -- | Dest
  OsPath ->
  m ()
copySymbolicLink src dest = do
  PR.throwIfWrongPathType "copySymbolicLink" PathTypeSymbolicLink src

  target <- getSymbolicLinkTarget src

  -- NOTE: The distinction between a directory vs. file link does not exist
  -- for Posix, so this logic is for Windows. We test if the target exists
  -- and is a directory, in which case we use createDirectoryLink. If the
  -- target is a file, symlink itself, or does not exist, we fall back to
  -- createFileLink.
  pathIsSymbolicDirectoryLink src >>= \case
    True -> createDirectoryLink target dest
    False -> createFileLink target dest
{-# INLINEABLE copySymbolicLink #-}

-- $if-exists
-- The @removeXIfExists@ functions should be understood as helper combinators
-- for the obvious @doesXExist -> removeX@ pattern. They should __not__ be
-- understood as a total "delete arbitrary path if it exists" pattern.
--
-- For instance, 'doesDirectoryExist' will return true if the /target/ of a
-- symbolic link is a directory, yet 'removeDirectory' will throw an exception.
-- Thus these functions should only be used when the type (file, dir, symlink)
-- of a (possibly non-extant) path is __known__.
