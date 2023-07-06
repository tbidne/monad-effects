{-# LANGUAGE CPP #-}

-- | Provides the 'MonadPosix' typeclass.
--
-- @since 0.1
module Effects.System.PosixCompat
  ( -- * Effect
    MonadPosix (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import GHC.Stack (HasCallStack)
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

-- TODO:
--
-- - Path vs. FilePath

-- | Class for unix-compat effects.
--
-- @since 0.1
class (Monad m) => MonadPosix m where
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