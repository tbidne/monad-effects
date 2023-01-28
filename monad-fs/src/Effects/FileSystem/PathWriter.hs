{-# LANGUAGE CPP #-}

-- | Provides the MonadPathWriter effect.
--
-- @since 0.1
module Effects.FileSystem.PathWriter
  ( -- * Effect
    MonadPathWriter (..),
    Path,

    -- * Misc
    copyDirectoryRecursive,
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- * Exceptions
    PathExistsException (..),
    PathDoesNotExistException (..),

    -- * Reexports
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Foldable (for_, traverse_)
import Data.Time (UTCTime (..))
import Effects.Exception (MonadMask, addCS, mask_, onException, throwWithCS)
import Effects.FileSystem.Path (Path, (</>))
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( doesDirectoryExist,
        doesFileExist,
        doesPathExist
      ),
    listDirectoryRecursive,
  )
import Effects.IORef
  ( MonadIORef (modifyIORef', newIORef, readIORef),
  )
import GHC.Stack (HasCallStack)
import System.Directory (Permissions (..))
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as FP
#else
import System.Directory qualified as Dir
import System.FilePath qualified as FP
#endif

-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => MonadPathWriter m where
  -- | Lifted 'Dir.createDirectory'.
  --
  -- @since 0.1
  createDirectory :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.createDirectoryIfMissing'.
  --
  -- @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> Path -> m ()

  -- | Lifted 'Dir.removeDirectory'.
  --
  -- @since 0.1
  removeDirectory :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.removeDirectoryRecursive'.
  --
  -- @since 0.1
  removeDirectoryRecursive :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.removePathForcibly'.
  --
  -- @since 0.1
  removePathForcibly :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.renameDirectory'.
  --
  -- @since 0.1
  renameDirectory :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.setCurrentDirectory'.
  --
  -- @since 0.1
  setCurrentDirectory :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.withCurrentDirectory'.
  --
  -- @since 0.1
  withCurrentDirectory :: HasCallStack => Path -> m a -> m a

  -- | Lifted 'Dir.removeFile'.
  --
  -- @since 0.1
  removeFile :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.renameFile'.
  --
  -- @since 0.1
  renameFile :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.renamePath'.
  --
  -- @since 0.1
  renamePath :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.copyFile'.
  --
  -- @since 0.1
  copyFile :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.copyFileWithMetadata'.
  --
  -- @since 0.1
  copyFileWithMetadata :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.createFileLink'.
  --
  -- @since 0.1
  createFileLink :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.createDirectoryLink'.
  --
  -- @since 0.1
  createDirectoryLink :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.removeDirectoryLink'.
  --
  -- @since 0.1
  removeDirectoryLink :: HasCallStack => Path -> m ()

  -- | Lifted 'Dir.setPermissions'.
  --
  -- @since 0.1
  setPermissions :: HasCallStack => Path -> Permissions -> m ()

  -- | Lifted 'Dir.copyPermissions'.
  --
  -- @since 0.1
  copyPermissions :: HasCallStack => Path -> Path -> m ()

  -- | Lifted 'Dir.setAccessTime'.
  --
  -- @since 0.1
  setAccessTime :: HasCallStack => Path -> UTCTime -> m ()

  -- | Lifted 'Dir.setModificationTime'.
  --
  -- @since 0.1
  setModificationTime :: HasCallStack => Path -> UTCTime -> m ()

-- | @since 0.1
instance MonadPathWriter IO where
  createDirectory = addCS . Dir.createDirectory
  {-# INLINEABLE createDirectory #-}
  createDirectoryIfMissing b = addCS . Dir.createDirectoryIfMissing b
  {-# INLINEABLE createDirectoryIfMissing #-}
  removeDirectory = addCS . Dir.removeDirectory
  {-# INLINEABLE removeDirectory #-}
  removeDirectoryRecursive = addCS . Dir.removeDirectoryRecursive
  {-# INLINEABLE removeDirectoryRecursive #-}
  removePathForcibly = addCS . Dir.removePathForcibly
  {-# INLINEABLE removePathForcibly #-}
  renameDirectory p = addCS . Dir.renameDirectory p
  {-# INLINEABLE renameDirectory #-}
  setCurrentDirectory = addCS . Dir.setCurrentDirectory
  {-# INLINEABLE setCurrentDirectory #-}
  withCurrentDirectory p = addCS . Dir.withCurrentDirectory p
  {-# INLINEABLE withCurrentDirectory #-}
  removeFile = addCS . Dir.removeFile
  {-# INLINEABLE removeFile #-}
  renameFile p = addCS . Dir.renameFile p
  {-# INLINEABLE renameFile #-}
  renamePath p = addCS . Dir.renamePath p
  {-# INLINEABLE renamePath #-}
  copyFile p = addCS . Dir.copyFile p
  {-# INLINEABLE copyFile #-}
  copyFileWithMetadata p = addCS . Dir.copyFileWithMetadata p
  {-# INLINEABLE copyFileWithMetadata #-}
  createFileLink p = addCS . Dir.createFileLink p
  {-# INLINEABLE createFileLink #-}
  createDirectoryLink p = addCS . Dir.createDirectoryLink p
  {-# INLINEABLE createDirectoryLink #-}
  removeDirectoryLink = addCS . Dir.removeDirectoryLink
  {-# INLINEABLE removeDirectoryLink #-}
  setPermissions p = addCS . Dir.setPermissions p
  {-# INLINEABLE setPermissions #-}
  copyPermissions p = addCS . Dir.copyPermissions p
  {-# INLINEABLE copyPermissions #-}
  setAccessTime p = addCS . Dir.setAccessTime p
  {-# INLINEABLE setAccessTime #-}
  setModificationTime p = addCS . Dir.setModificationTime p
  {-# INLINEABLE setModificationTime #-}

-- | @since 0.1
instance MonadPathWriter m => MonadPathWriter (ReaderT env m) where
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

-- | Exception for trying to create a path that already exists.
--
-- @since 0.1
newtype PathExistsException = MkPathExistsException Path
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathExistsException where
  displayException (MkPathExistsException path) =
    "Path already exists: " <> path

-- | Exception for trying to create a path that already exists.
--
-- @since 0.1
newtype PathDoesNotExistException = MkPathDoesNotExistException Path
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathDoesNotExistException where
  displayException (MkPathDoesNotExistException path) =
    "Path does not exist: " <> path

-- | @copyDirectoryRecursive overwrite src dest@ copies the @src@ and its
-- contents into @dest@ e.g.
--
-- @
-- copyDirectoryRecursive b "path\/to\/foo" "path\/to\/bar"
-- @
--
-- will create @path\/to\/bar\/foo@.
--
-- __Throws:__
--
-- * 'PathDoesNotExistException': if @dest@ does not exist.
-- * 'PathExistsException': if @dest/\<src\>@ exists and @overwrite@ is false.
--
-- This function attempts to be atomic, in that if an error occurs during
-- the copying, we try to roll back any successful writes.
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
  -- | Overwrite
  Bool ->
  -- | Source
  Path ->
  -- | Destination
  Path ->
  m ()
copyDirectoryRecursive overwrite src destRoot = do
  destExists <- doesDirectoryExist destRoot

  unless destExists $
    throwWithCS $
      MkPathDoesNotExistException destRoot

  let srcName = FP.takeBaseName $ FP.dropTrailingPathSeparator src
      dest = destRoot </> srcName

  if overwrite
    then copyDirectoryOverwrite src dest
    else copyDirectory src dest

copyDirectoryOverwrite ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Source
  Path ->
  -- | Destination
  Path ->
  m ()
copyDirectoryOverwrite src dest = do
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

  destExists <- doesDirectoryExist dest

  let copyFiles = do
        (subFiles, subDirs) <- listDirectoryRecursive src

        -- create dest if it does not exist
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
          -- TODO: handle file collisions?
          copyFileWithMetadata (src </> f) (dest </> f)
          modifyIORef' copiedFilesRef ((dest </> f) :)

      cleanup =
        if destExists
          then do
            -- manually delete files and dirs
            readIORef copiedFilesRef >>= traverse_ removeFile
            readIORef createdDirsRef >>= traverse_ removeDirectory
          else removeDirectoryRecursive dest

  copyFiles `onException` mask_ cleanup

copyDirectory ::
  forall m.
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | Source
  Path ->
  -- | Destination
  Path ->
  m ()
copyDirectory src dest = do
  destExists <- doesDirectoryExist dest
  when destExists $ throwWithCS (MkPathExistsException dest)

  let copyFiles = do
        (subFiles, subDirs) <- listDirectoryRecursive src
        createDirectory dest

        -- create intermediate dirs if they do not exist
        traverse_ (createDirectoryIfMissing True . (dest </>)) subDirs

        -- copy files
        for_ subFiles $ \f -> copyFileWithMetadata (src </> f) (dest </> f)

      -- delete directory
      cleanup = removeDirectoryRecursive dest

  copyFiles `onException` mask_ cleanup

-- | Calls 'removeFile' if 'doesFileExist' is 'True'.
--
-- @since 0.1
removeFileIfExists ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  Path ->
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
  Path ->
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
  Path ->
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
  Path ->
  m ()
removePathForciblyIfExists =
  removeIfExists doesPathExist removePathForcibly
{-# INLINEABLE removePathForciblyIfExists #-}

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
{-# INLINEABLE removeIfExists #-}
