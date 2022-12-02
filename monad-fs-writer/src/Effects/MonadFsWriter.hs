{-# LANGUAGE CPP #-}

-- | Provides the 'MonadFsWriter' typeclass.
--
-- @since 0.1
module Effects.MonadFsWriter
  ( MonadFsWriter (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effects.MonadCallStack (checkpointCallStack)
import GHC.Stack (HasCallStack)
#if MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath (OsPath)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import Prelude hiding (appendFile, writeFile)

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

  -- | Removes a path.
  --
  -- @since 0.1
  removePathForcibly :: HasCallStack => Path -> m ()

  -- | Removes a directory.
  --
  -- @since 0.1
  removeDirectoryRecursive :: HasCallStack => Path -> m ()

  -- | Creates a directory.
  --
  -- @since 0.1
  createDirectoryIfMissing :: HasCallStack => Bool -> Path -> m ()

-- | @since 0.1
instance MonadFsWriter IO where
  writeFile f = checkpointCallStack . BS.writeFile f
  appendFile f = checkpointCallStack . BS.appendFile f
  openFile f = checkpointCallStack . IO.openFile f
  hPut h = checkpointCallStack . BS.hPut h
  hClose = checkpointCallStack . IO.hClose
  hFlush = checkpointCallStack . IO.hFlush
  renameFile f = checkpointCallStack . Dir.renameFile f
  removeFile = checkpointCallStack . Dir.removeFile
  renameDirectory f = checkpointCallStack . Dir.renameDirectory f
  removePathForcibly = checkpointCallStack . Dir.removePathForcibly
  removeDirectoryRecursive = checkpointCallStack . Dir.removeDirectoryRecursive
  createDirectoryIfMissing b = checkpointCallStack . Dir.createDirectoryIfMissing b

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
  removePathForcibly = lift . removePathForcibly
  removeDirectoryRecursive = lift . removeDirectoryRecursive
  createDirectoryIfMissing b = lift . createDirectoryIfMissing b
