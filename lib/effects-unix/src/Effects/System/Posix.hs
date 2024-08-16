{-# OPTIONS_GHC -Wno-unused-imports #-}

-- NOTE: [Unused FilePath]
--
-- -Wno-unused-imports due to the "unused" System.FilePath import
-- below. We import it because in case we are using the new os-string
-- package, we need to ensure we are using filepath >= 1.5, as lower
-- versions provide their own incompatible os-string. But filepath is
-- a transitive dep, so the only way to do this is add an "unused" dependency
-- in the cabal file.
--
-- We attempted to disable the warning more appropriately with
--
--   ghc-options: -Wno-unused-packages
--
-- in the cabal file, but this didn't work for some reason. Hence this
-- workaround here. We should be able to remove all of this once nix can
-- use GHC 9.10.
--
-- See TODO: [FilePath vs. OsString upgrade].

-- | Provides the 'MonadPosix' typeclass.
--
-- @since 0.1
module Effects.System.Posix
  ( -- * Effect
    MonadPosix (..),

    -- * PathType
    PathType (..),

    -- ** Functions
    displayPathType,
    throwIfWrongPathType,
    isPathType,
    getPathType,
  )
where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor ((<&>))
import Effects.Exception (MonadCatch)
import Effects.FileSystem.IO qualified as FS.IO
import Effects.FileSystem.PathType
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
import System.FilePath qualified
import System.OsString.Internal.Types (OsString (OsString))
import System.Posix.Files.PosixString (FileStatus, PathVar)
import System.Posix.Files.PosixString qualified as PFiles
import System.Posix.PosixString (PosixPath)
import System.Posix.Types
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

-- | Class for unix effects.
--
-- @since 0.1
class (Monad m) => MonadPosix m where
  -- System.Posix.Files

  -- | @since 0.1
  setFileMode :: (HasCallStack) => PosixPath -> FileMode -> m ()

  -- | @since 0.1
  setFdMode :: (HasCallStack) => Fd -> FileMode -> m ()

  -- | @since 0.1
  setFileCreationMask :: (HasCallStack) => FileMode -> m FileMode

  -- | @since 0.1
  fileAccess :: (HasCallStack) => PosixPath -> Bool -> Bool -> Bool -> m Bool

  -- | @since 0.1
  fileExist :: (HasCallStack) => PosixPath -> m Bool

  -- | @since 0.1
  getFileStatus :: (HasCallStack) => PosixPath -> m FileStatus

  -- | @since 0.1
  getFdStatus :: (HasCallStack) => Fd -> m FileStatus

  -- | @since 0.1
  getSymbolicLinkStatus :: (HasCallStack) => PosixPath -> m FileStatus

  -- | @since 0.1
  createNamedPipe :: (HasCallStack) => PosixPath -> FileMode -> m ()

  -- | @since 0.1
  createDevice :: (HasCallStack) => PosixPath -> FileMode -> DeviceID -> m ()

  -- | @since 0.1
  createLink :: (HasCallStack) => PosixPath -> PosixPath -> m ()

  -- | @since 0.1
  removeLink :: (HasCallStack) => PosixPath -> m ()

  -- | @since 0.1
  createSymbolicLink :: (HasCallStack) => PosixPath -> PosixPath -> m ()

  -- | @since 0.1
  readSymbolicLink :: (HasCallStack) => PosixPath -> m PosixPath

  -- | @since 0.1
  rename :: (HasCallStack) => PosixPath -> PosixPath -> m ()

  -- | @since 0.1
  setOwnerAndGroup :: (HasCallStack) => PosixPath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFdOwnerAndGroup :: (HasCallStack) => Fd -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setSymbolicLinkOwnerAndGroup :: (HasCallStack) => PosixPath -> UserID -> GroupID -> m ()

  -- | @since 0.1
  setFileTimes :: (HasCallStack) => PosixPath -> EpochTime -> EpochTime -> m ()

  -- | @since 0.1
  touchFile :: (HasCallStack) => PosixPath -> m ()

  -- | @since 0.1
  setFileSize :: (HasCallStack) => PosixPath -> FileOffset -> m ()

  -- | @since 0.1
  setFdSize :: (HasCallStack) => Fd -> FileOffset -> m ()

  -- | @since 0.1
  getPathVar :: (HasCallStack) => PosixPath -> PathVar -> m Limit

  -- | @since 0.1
  getFdPathVar :: (HasCallStack) => Fd -> PathVar -> m Limit

-- | @since 0.1
instance MonadPosix IO where
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
instance (MonadPosix m) => MonadPosix (ReaderT e m) where
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

-- | Throws 'IOException' if the path does not exist or the expected path type
-- does not match actual.
--
-- @since 0.1
throwIfWrongPathType ::
  ( HasCallStack,
    MonadCatch m,
    MonadPosix m
  ) =>
  String ->
  PathType ->
  PosixPath ->
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
      (OsString path)
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
    MonadPosix m
  ) =>
  PathType ->
  PosixPath ->
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
    MonadPosix m
  ) =>
  PosixPath ->
  m PathType
getPathType path =
  -- NOTE: We use getSymbolicLinkStatus instead of getFileStatus because
  -- the latter follows symlinks, which we do not want.
  getSymbolicLinkStatus path <&> \status ->
    if
      | PFiles.isSymbolicLink status -> PathTypeSymbolicLink
      | PFiles.isDirectory status -> PathTypeDirectory
      | PFiles.isRegularFile status -> PathTypeFile
      | otherwise -> PathTypeOther
{-# INLINEABLE getPathType #-}
