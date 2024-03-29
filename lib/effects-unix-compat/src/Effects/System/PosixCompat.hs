-- | Provides the 'MonadPosixCompat' typeclass.
--
-- @since 0.1
module Effects.System.PosixCompat
  ( -- * Effect
    MonadPosixCompat (..),

    -- * PathType
    PathType (..),
    displayPathType,

    -- ** Functions
    throwIfWrongPathType,
    isPathType,
    getPathType,

    -- ** Optics
    _PathTypeFile,
    _PathTypeDirectory,
    _PathTypeSymbolicLink,
    _PathTypeOther,

    -- * Utils
    throwPathIOError,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor ((<&>))
import Data.String (IsString)
import Effects.Exception (MonadCatch, MonadThrow, throwCS)
import GHC.Generics (Generic)
import GHC.IO.Exception
  ( IOErrorType (InappropriateType),
    IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
import GHC.Stack (HasCallStack)
import Optics.Core (Prism', prism)
import System.PosixCompat.Files (FileStatus, PathVar)
import System.PosixCompat.Files qualified as PFiles
import System.PosixCompat.Types
  ( DeviceID,
    EpochTime,
    Fd,
    FileMode,
    FileOffset,
    GroupID,
    Limit,
    UserID,
  )

{- HLINT ignore "Redundant bracket" -}

-- | Class for unix-compat effects.
--
-- @since 0.1
class (Monad m) => MonadPosixCompat m where
  -- System.PosixCompat.Files

  -- | @since 0.1
  setFileMode :: (HasCallStack) => FilePath -> FileMode -> m ()

  -- | @since 0.1
  setFdMode :: (HasCallStack) => Fd -> FileMode -> m ()

  -- | @since 0.1
  setFileCreationMask :: (HasCallStack) => FileMode -> m FileMode

  -- | @since 0.1
  fileAccess :: (HasCallStack) => FilePath -> Bool -> Bool -> Bool -> m Bool

  -- | @since 0.1
  fileExist :: (HasCallStack) => FilePath -> m Bool

  -- | @since 0.1
  getFileStatus :: (HasCallStack) => FilePath -> m FileStatus

  -- | @since 0.1
  getFdStatus :: (HasCallStack) => Fd -> m FileStatus

  -- | @since 0.1
  getSymbolicLinkStatus :: (HasCallStack) => FilePath -> m FileStatus

  -- | @since 0.1
  createNamedPipe :: (HasCallStack) => FilePath -> FileMode -> m ()

  -- | @since 0.1
  createDevice :: (HasCallStack) => FilePath -> FileMode -> DeviceID -> m ()

  -- | @since 0.1
  createLink :: (HasCallStack) => FilePath -> FilePath -> m ()

  -- | @since 0.1
  removeLink :: (HasCallStack) => FilePath -> m ()

  -- | @since 0.1
  createSymbolicLink :: (HasCallStack) => FilePath -> FilePath -> m ()

  -- | @since 0.1
  readSymbolicLink :: (HasCallStack) => FilePath -> m FilePath

  -- | @since 0.1
  rename :: (HasCallStack) => FilePath -> FilePath -> m ()

  -- | @since 0.1
  setOwnerAndGroup :: (HasCallStack) => FilePath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFdOwnerAndGroup :: (HasCallStack) => Fd -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setSymbolicLinkOwnerAndGroup :: (HasCallStack) => FilePath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFileTimes :: (HasCallStack) => FilePath -> EpochTime -> EpochTime -> m ()

  -- | @since 0.1
  touchFile :: (HasCallStack) => FilePath -> m ()

  -- | @since 0.1
  setFileSize :: (HasCallStack) => FilePath -> FileOffset -> m ()

  -- | @since 0.1
  setFdSize :: (HasCallStack) => Fd -> FileOffset -> m ()

  -- | @since 0.1
  getPathVar :: (HasCallStack) => FilePath -> PathVar -> m Limit

  -- | @since 0.1
  getFdPathVar :: (HasCallStack) => Fd -> PathVar -> m Limit

-- | @since 0.1
instance MonadPosixCompat IO where
  setFileMode = PFiles.setFileMode
  {-# INLINEABLE setFileMode #-}
  setFdMode = PFiles.setFdMode
  {-# INLINEABLE setFdMode #-}
  setFileCreationMask = PFiles.setFileCreationMask
  {-# INLINEABLE setFileCreationMask #-}
  fileAccess = PFiles.fileAccess
  {-# INLINEABLE fileAccess #-}
  fileExist = PFiles.fileExist
  {-# INLINEABLE fileExist #-}
  getFileStatus = PFiles.getFileStatus
  {-# INLINEABLE getFileStatus #-}
  getFdStatus = PFiles.getFdStatus
  {-# INLINEABLE getFdStatus #-}
  getSymbolicLinkStatus = PFiles.getSymbolicLinkStatus
  {-# INLINEABLE getSymbolicLinkStatus #-}
  createNamedPipe = PFiles.createNamedPipe
  {-# INLINEABLE createNamedPipe #-}
  createDevice = PFiles.createDevice
  {-# INLINEABLE createDevice #-}
  createLink = PFiles.createLink
  {-# INLINEABLE createLink #-}
  removeLink = PFiles.removeLink
  {-# INLINEABLE removeLink #-}
  createSymbolicLink = PFiles.createSymbolicLink
  {-# INLINEABLE createSymbolicLink #-}
  readSymbolicLink = PFiles.readSymbolicLink
  {-# INLINEABLE readSymbolicLink #-}
  rename = PFiles.rename
  {-# INLINEABLE rename #-}
  setOwnerAndGroup = PFiles.setOwnerAndGroup
  {-# INLINEABLE setOwnerAndGroup #-}
  setFdOwnerAndGroup = PFiles.setFdOwnerAndGroup
  {-# INLINEABLE setFdOwnerAndGroup #-}
  setSymbolicLinkOwnerAndGroup = PFiles.setSymbolicLinkOwnerAndGroup
  {-# INLINEABLE setSymbolicLinkOwnerAndGroup #-}
  setFileTimes = PFiles.setFileTimes
  {-# INLINEABLE setFileTimes #-}
  touchFile = PFiles.touchFile
  {-# INLINEABLE touchFile #-}
  setFileSize = PFiles.setFileSize
  {-# INLINEABLE setFileSize #-}
  setFdSize = PFiles.setFdSize
  {-# INLINEABLE setFdSize #-}
  getPathVar = PFiles.getPathVar
  {-# INLINEABLE getPathVar #-}
  getFdPathVar = PFiles.getFdPathVar
  {-# INLINEABLE getFdPathVar #-}

-- | @since 0.1
instance (MonadPosixCompat m) => MonadPosixCompat (ReaderT e m) where
  setFileMode p = lift . setFileMode p
  {-# INLINEABLE setFileMode #-}
  setFdMode fd = lift . setFdMode fd
  {-# INLINEABLE setFdMode #-}
  setFileCreationMask = lift . setFileCreationMask
  {-# INLINEABLE setFileCreationMask #-}
  fileAccess p a b = lift . fileAccess p a b
  {-# INLINEABLE fileAccess #-}
  fileExist = lift . fileExist
  {-# INLINEABLE fileExist #-}
  getFileStatus = lift . getFileStatus
  {-# INLINEABLE getFileStatus #-}
  getFdStatus = lift . getFdStatus
  {-# INLINEABLE getFdStatus #-}
  getSymbolicLinkStatus = lift . getSymbolicLinkStatus
  {-# INLINEABLE getSymbolicLinkStatus #-}
  createNamedPipe p = lift . createNamedPipe p
  {-# INLINEABLE createNamedPipe #-}
  createDevice p m = lift . createDevice p m
  {-# INLINEABLE createDevice #-}
  createLink p = lift . createLink p
  {-# INLINEABLE createLink #-}
  removeLink = lift . removeLink
  {-# INLINEABLE removeLink #-}
  createSymbolicLink p = lift . createSymbolicLink p
  {-# INLINEABLE createSymbolicLink #-}
  readSymbolicLink = lift . readSymbolicLink
  {-# INLINEABLE readSymbolicLink #-}
  rename p = lift . rename p
  {-# INLINEABLE rename #-}
  setOwnerAndGroup p u = lift . setOwnerAndGroup p u
  {-# INLINEABLE setOwnerAndGroup #-}
  setFdOwnerAndGroup fd u = lift . setFdOwnerAndGroup fd u
  {-# INLINEABLE setFdOwnerAndGroup #-}
  setSymbolicLinkOwnerAndGroup p u = lift . setSymbolicLinkOwnerAndGroup p u
  {-# INLINEABLE setSymbolicLinkOwnerAndGroup #-}
  setFileTimes p t = lift . setFileTimes p t
  {-# INLINEABLE setFileTimes #-}
  touchFile = lift . touchFile
  {-# INLINEABLE touchFile #-}
  setFileSize p = lift . setFileSize p
  {-# INLINEABLE setFileSize #-}
  setFdSize fd = lift . setFdSize fd
  {-# INLINEABLE setFdSize #-}
  getPathVar p = lift . getPathVar p
  {-# INLINEABLE getPathVar #-}
  getFdPathVar fd = lift . getFdPathVar fd
  {-# INLINEABLE getFdPathVar #-}

-- | Path type.
--
-- @since 0.1
data PathType
  = -- | @since 0.1
    PathTypeFile
  | -- | @since 0.1
    PathTypeDirectory
  | -- | @since 0.1
    PathTypeSymbolicLink
  | -- | @since 0.1
    PathTypeOther
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_PathTypeFile :: Prism' PathType ()
_PathTypeFile =
  prism
    (const PathTypeFile)
    ( \case
        PathTypeFile -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeFile #-}

-- | @since 0.1
_PathTypeDirectory :: Prism' PathType ()
_PathTypeDirectory =
  prism
    (const PathTypeDirectory)
    ( \case
        PathTypeDirectory -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeDirectory #-}

-- | @since 0.1
_PathTypeSymbolicLink :: Prism' PathType ()
_PathTypeSymbolicLink =
  prism
    (const PathTypeSymbolicLink)
    ( \case
        PathTypeSymbolicLink -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeSymbolicLink #-}

-- | @since 0.1
_PathTypeOther :: Prism' PathType ()
_PathTypeOther =
  prism
    (const PathTypeOther)
    ( \case
        PathTypeOther -> Right ()
        x -> Left x
    )
{-# INLINE _PathTypeOther #-}

-- | String representation of 'PathType'.
--
-- @since 0.1
displayPathType :: (IsString a) => PathType -> a
displayPathType PathTypeFile = "file"
displayPathType PathTypeDirectory = "directory"
displayPathType PathTypeSymbolicLink = "symlink"
displayPathType PathTypeOther = "other"

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPosixCompat m
  ) =>
  String ->
  PathType ->
  FilePath ->
  m ()
throwIfWrongPathType location expected path = do
  actual <- getPathType path

  let err =
        mconcat
          [ "Expected path '",
            path,
            "' to have type ",
            displayPathType expected,
            ", but detected ",
            displayPathType actual
          ]

  unless (expected == actual) $
    throwPathIOError
      path
      location
      InappropriateType
      err
{-# INLINEABLE throwIfWrongPathType #-}

-- | Checks that the path type matches the expectation. Throws
-- 'IOException' if the path does not exist or the type cannot be detected.
--
-- @since 0.1
isPathType ::
  ( HasCallStack,
    MonadPosixCompat m
  ) =>
  PathType ->
  FilePath ->
  m Bool
isPathType expected = fmap (== expected) . getPathType
{-# INLINEABLE isPathType #-}

-- | Returns the type for a given path without following symlinks.
-- Throws 'IOException' if the path does not exist or the type cannot be
-- detected.
--
-- @since 0.1
getPathType ::
  ( HasCallStack,
    MonadPosixCompat m
  ) =>
  FilePath ->
  m PathType
getPathType path = do
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther
{-# INLINEABLE getPathType #-}

-- | Helper for throwing 'IOException'.
--
-- @since 0.1
throwPathIOError ::
  (HasCallStack, MonadThrow m) =>
  -- | Path upon which the IO operation failed.
  FilePath ->
  -- | String location (e.g. function name).
  String ->
  -- | Type of exception.
  IOErrorType ->
  -- | Description.
  String ->
  m a
throwPathIOError path loc ty desc =
  throwCS $
    IOError
      { ioe_handle = Nothing,
        ioe_type = ty,
        ioe_location = loc,
        ioe_description = desc,
        ioe_errno = Nothing,
        ioe_filename = Just path
      }
{-# INLINEABLE throwPathIOError #-}
