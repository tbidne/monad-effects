-- | Provides the 'MonadFsReader' typeclass.
--
-- @since 0.1
module Effects.MonadFsReader
  ( MonadFsReader (..),
  )
where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Effects.MonadCallStack (checkpointCallStack)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import System.Directory (XdgDirectory (XdgConfig))
import System.Directory qualified as Dir
import Prelude hiding (readFile)

-- | Represents thread effects.
--
-- @since 0.1
class Monad m => MonadFsReader m where
  -- | Retrieves the file size in bytes.
  --
  -- @since 0.1
  getFileSize :: HasCallStack => FilePath -> m Natural

  -- | Retrieves the file size in bytes.
  --
  -- @since 0.1
  -- getPathSize :: HasCallStack => FilePath -> m

  -- | Returns the default directory e.g. Xdg config dir.
  --
  -- @since 0.1
  getXdgConfig :: HasCallStack => FilePath -> m FilePath

  -- | Reads a file.
  --
  -- @since 0.1
  readFile :: HasCallStack => FilePath -> m ByteString

  -- | Tests a file's existence.
  --
  -- @since 0.1
  doesFileExist :: HasCallStack => FilePath -> m Bool

  -- | Tests a directory's existence.
  --
  -- @since 0.1
  doesDirectoryExist :: HasCallStack => FilePath -> m Bool

  -- | Tests a path's existence.
  --
  -- @since 0.1
  doesPathExist :: HasCallStack => FilePath -> m Bool

  -- | Canonicalize a path.
  --
  -- @since 0.1
  canonicalizePath :: HasCallStack => FilePath -> m FilePath

  -- | Lists a directory.
  --
  -- @since 0.1
  listDirectory :: HasCallStack => FilePath -> m [FilePath]

-- | @since 0.1
instance MonadFsReader IO where
  getFileSize = checkpointCallStack . fmap fromIntegral . Dir.getFileSize

  -- getPathSize f = getPathSize f

  getXdgConfig = checkpointCallStack . Dir.getXdgDirectory XdgConfig
  readFile = checkpointCallStack . BS.readFile
  doesFileExist = checkpointCallStack . Dir.doesFileExist
  doesDirectoryExist = checkpointCallStack . Dir.doesDirectoryExist
  doesPathExist = checkpointCallStack . Dir.doesPathExist
  canonicalizePath = checkpointCallStack . Dir.canonicalizePath
  listDirectory = checkpointCallStack . Dir.listDirectory

-- | @since 0.1
instance MonadFsReader m => MonadFsReader (ReaderT e m) where
  getFileSize = lift . getFileSize
  getXdgConfig = lift . getXdgConfig
  readFile = lift . readFile
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  doesPathExist = lift . doesPathExist
  canonicalizePath = lift . canonicalizePath
  listDirectory = lift . listDirectory
