{-# LANGUAGE CPP #-}

-- | Provides the MonadPathWriter effect.
--
-- @since 0.1
module Effects.FileSystem.MonadPathWriter
  ( -- * Class
    MonadPathWriter (..),
    Path,

    -- * Misc
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,

    -- * Reexports
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time (UTCTime (..))
import Effects.FileSystem.MonadPathReader
  ( MonadPathReader
      ( doesDirectoryExist,
        doesFileExist,
        doesPathExist
      ),
  )
import Effects.FileSystem.Path (Path)
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Stack (HasCallStack)
import System.Directory (Permissions (..))
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif

-- REVIEW: Can we reduce the class size by implementing some of these
-- functions in terms of others?

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
  createDirectory = addCallStack . Dir.createDirectory
  {-# INLINEABLE createDirectory #-}
  createDirectoryIfMissing b = addCallStack . Dir.createDirectoryIfMissing b
  {-# INLINEABLE createDirectoryIfMissing #-}
  removeDirectory = addCallStack . Dir.removeDirectory
  {-# INLINEABLE removeDirectory #-}
  removeDirectoryRecursive = addCallStack . Dir.removeDirectoryRecursive
  {-# INLINEABLE removeDirectoryRecursive #-}
  removePathForcibly = addCallStack . Dir.removePathForcibly
  {-# INLINEABLE removePathForcibly #-}
  renameDirectory p = addCallStack . Dir.renameDirectory p
  {-# INLINEABLE renameDirectory #-}
  setCurrentDirectory = addCallStack . Dir.setCurrentDirectory
  {-# INLINEABLE setCurrentDirectory #-}
  withCurrentDirectory p = addCallStack . Dir.withCurrentDirectory p
  {-# INLINEABLE withCurrentDirectory #-}
  removeFile = addCallStack . Dir.removeFile
  {-# INLINEABLE removeFile #-}
  renameFile p = addCallStack . Dir.renameFile p
  {-# INLINEABLE renameFile #-}
  renamePath p = addCallStack . Dir.renamePath p
  {-# INLINEABLE renamePath #-}
  copyFile p = addCallStack . Dir.copyFile p
  {-# INLINEABLE copyFile #-}
  copyFileWithMetadata p = addCallStack . Dir.copyFileWithMetadata p
  {-# INLINEABLE copyFileWithMetadata #-}
  createFileLink p = addCallStack . Dir.createFileLink p
  {-# INLINEABLE createFileLink #-}
  createDirectoryLink p = addCallStack . Dir.createDirectoryLink p
  {-# INLINEABLE createDirectoryLink #-}
  removeDirectoryLink = addCallStack . Dir.removeDirectoryLink
  {-# INLINEABLE removeDirectoryLink #-}
  setPermissions p = addCallStack . Dir.setPermissions p
  {-# INLINEABLE setPermissions #-}
  copyPermissions p = addCallStack . Dir.copyPermissions p
  {-# INLINEABLE copyPermissions #-}
  setAccessTime p = addCallStack . Dir.setAccessTime p
  {-# INLINEABLE setAccessTime #-}
  setModificationTime p = addCallStack . Dir.setModificationTime p
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
