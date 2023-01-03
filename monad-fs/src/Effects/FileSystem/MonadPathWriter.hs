{-# LANGUAGE CPP #-}

-- | Provides the FileSystem effect.
--
-- @since 0.1
module Effects.FileSystem.MonadPathWriter
  ( -- * Class
    MonadPathWriter (..),
    Path,

    -- * Reexports
    Permissions (..),
    UTCTime (..),

    -- * Misc
    removeFileIfExists,
    removeDirectoryIfExists,
    removeDirectoryRecursiveIfExists,
    removePathForciblyIfExists,
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Time (UTCTime (..))
import Effects.FileSystem.IO.File.MonadFileReader (Path)
import Effects.FileSystem.MonadPathReader (MonadPathReader (doesDirectoryExist, doesFileExist, doesPathExist))
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack),
  )
import GHC.Stack (HasCallStack)
import System.Directory (Permissions (..))
import System.Directory qualified as Dir

-- | Represents file-system writer effects.
--
-- @since 0.1
class Monad m => MonadPathWriter m where
  -- | @since 0.1
  createDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> Path -> m ()

  -- | @since 0.1
  removeDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  removeDirectoryRecursive :: HasCallStack => Path -> m ()

  -- | @since 0.1
  removePathForcibly :: HasCallStack => Path -> m ()

  -- | @since 0.1
  renameDirectory :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  setCurrentDirectory :: HasCallStack => Path -> m ()

  -- | @since 0.1
  withCurrentDirectory :: HasCallStack => Path -> m a -> m a

  -- | @since 0.1
  removeFile :: HasCallStack => Path -> m ()

  -- | @since 0.1
  renameFile :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  renamePath :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  copyFile :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  copyFileWithMetadata :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  createFileLink :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  createDirectoryLink :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  removeDirectoryLink :: HasCallStack => Path -> m ()

  -- | @since 0.1
  setPermissions :: HasCallStack => Path -> Permissions -> m ()

  -- | @since 0.1
  copyPermissions :: HasCallStack => Path -> Path -> m ()

  -- | @since 0.1
  setAccessTime :: HasCallStack => Path -> UTCTime -> m ()

  -- | @since 0.1
  setModificationTime :: HasCallStack => Path -> UTCTime -> m ()

-- | @since 0.1
instance MonadPathWriter IO where
  createDirectory = addCallStack . Dir.createDirectory
  createDirectoryIfMissing b = addCallStack . Dir.createDirectoryIfMissing b
  removeDirectory = addCallStack . Dir.removeDirectory
  removeDirectoryRecursive = addCallStack . Dir.removeDirectoryRecursive
  removePathForcibly = addCallStack . Dir.removePathForcibly
  renameDirectory p = addCallStack . Dir.renameDirectory p
  setCurrentDirectory = addCallStack . Dir.setCurrentDirectory
  withCurrentDirectory p = addCallStack . Dir.withCurrentDirectory p
  removeFile = addCallStack . Dir.removeFile
  renameFile p = addCallStack . Dir.renameFile p
  renamePath p = addCallStack . Dir.renamePath p
  copyFile p = addCallStack . Dir.copyFile p
  copyFileWithMetadata p = addCallStack . Dir.copyFileWithMetadata p
  createFileLink p = addCallStack . Dir.createFileLink p
  createDirectoryLink p = addCallStack . Dir.createDirectoryLink p
  removeDirectoryLink = addCallStack . Dir.removeDirectoryLink
  setPermissions p = addCallStack . Dir.setPermissions p
  copyPermissions p = addCallStack . Dir.copyPermissions p
  setAccessTime p = addCallStack . Dir.setAccessTime p
  setModificationTime p = addCallStack . Dir.setModificationTime p

-- | @since 0.1
instance MonadPathWriter m => MonadPathWriter (ReaderT env m) where
  createDirectory = lift . createDirectory
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
  removeDirectory = lift . removeDirectory
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  removePathForcibly = lift . removePathForcibly
  renameDirectory p = lift . renameDirectory p
  setCurrentDirectory = lift . setCurrentDirectory
  withCurrentDirectory p action =
    ask >>= lift . \e -> withCurrentDirectory p (runReaderT action e)
  removeFile = lift . removeFile
  renameFile p = lift . renameFile p
  renamePath p = lift . renamePath p
  copyFile p = lift . copyFile p
  copyFileWithMetadata p = lift . copyFileWithMetadata p
  createFileLink p = lift . createFileLink p
  createDirectoryLink p = lift . createDirectoryLink p
  removeDirectoryLink = lift . removeDirectoryLink
  setPermissions p = lift . setPermissions p
  copyPermissions p = lift . copyPermissions p
  setAccessTime p = lift . setAccessTime p
  setModificationTime p = lift . setModificationTime p

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

removeIfExists :: Monad m => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
