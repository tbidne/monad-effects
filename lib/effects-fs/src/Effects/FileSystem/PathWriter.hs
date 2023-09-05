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

    -- ** Optics
    _OverwriteNone,
    _OverwriteDirectories,
    _OverwriteAll,
    _TargetNameSrc,
    _TargetNameLiteral,
    _TargetNameDest,

    -- * Removing
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

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Foldable (for_, traverse_)
import Data.Time (UTCTime (..))
import Effects.Exception (MonadMask, addCS, mask_, onException, throwCS)
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( doesDirectoryExist,
        doesFileExist,
        doesPathExist
      ),
    listDirectoryRecursive,
  )
import Effects.FileSystem.Utils (OsPath, (</>))
import Effects.IORef
  ( MonadIORef (modifyIORef', newIORef, readIORef),
  )
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
import System.Directory (Permissions (..))
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as FP

-- | Represents file-system writer effects.
--
-- @since 0.1
class (Monad m) => MonadPathWriter m where
  -- | @'createDirectory' dir@ creates a new directory @dir@ which is
  -- initially empty, or as near to empty as the operating system
  -- allows.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES]@
  --
  -- * 'isAlreadyExistsError'
  -- The operand refers to a directory that already exists.
  -- @ [EEXIST]@
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- The operand is not a valid directory name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- There is no path to the directory.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'System.IO.isFullError'
  -- Insufficient resources (virtual memory, process file descriptors,
  -- physical disk space, etc.) are available to perform the operation.
  -- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
  --
  -- * @InappropriateType@
  -- The path refers to an existing non-directory object.
  -- @[EEXIST]@
  --
  -- @since 0.1
  createDirectory :: (HasCallStack) => OsPath -> m ()

  -- | @'createDirectoryIfMissing' parents dir@ creates a new directory
  -- @dir@ if it doesn\'t exist. If the first argument is 'True'
  -- the function will also create all parent directories if they are missing.
  --
  -- @since 0.1
  createDirectoryIfMissing ::
    (HasCallStack) =>
    -- | Create its parents too?
    Bool ->
    -- | The path to the directory you want to make
    OsPath ->
    m ()

  -- | @'removeDirectory' dir@ removes an existing directory /dir/. The
  -- implementation may specify additional constraints which must be
  -- satisfied before a directory can be removed (e.g. the directory has to
  -- be empty, or may not be in use by other processes). It is not legal
  -- for an implementation to partially remove a directory unless the
  -- entire directory is removed. A conformant implementation need not
  -- support directory removal in all situations (e.g. removal of the root
  -- directory).
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- The operand is not a valid directory name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The directory does not exist.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES, EPERM]@
  --
  -- * @UnsatisfiedConstraints@
  -- Implementation-dependent constraints are not satisfied.
  -- @[EBUSY, ENOTEMPTY, EEXIST]@
  --
  -- * @UnsupportedOperation@
  -- The implementation does not support removal in this situation.
  -- @[EINVAL]@
  --
  -- * @InappropriateType@
  -- The operand refers to an existing non-directory object.
  -- @[ENOTDIR]@
  --
  -- @since 0.1
  removeDirectory :: (HasCallStack) => OsPath -> m ()

  -- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
  -- together with its contents and subdirectories. Within this directory,
  -- symbolic links are removed without affecting their targets.
  --
  -- On Windows, the operation fails if /dir/ is a directory symbolic link.
  --
  -- This operation is reported to be flaky on Windows so retry logic may
  -- be advisable. See: https://github.com/haskell/directory/pull/108
  --
  -- @since 0.1
  removeDirectoryRecursive :: (HasCallStack) => OsPath -> m ()

  -- | Removes a file or directory at /path/ together with its contents and
  -- subdirectories. Symbolic links are removed without affecting their
  -- targets. If the path does not exist, nothing happens.
  --
  -- Unlike other removal functions, this function will also attempt to delete
  -- files marked as read-only or otherwise made unremovable due to permissions.
  -- As a result, if the removal is incomplete, the permissions or attributes on
  -- the remaining files may be altered. If there are hard links in the
  -- directory, then permissions on all related hard links may be altered.
  --
  -- If an entry within the directory vanishes while @removePathForcibly@ is
  -- running, it is silently ignored.
  --
  -- If an exception occurs while removing an entry, @removePathForcibly@ will
  -- still try to remove as many entries as it can before failing with an
  -- exception. The first exception that it encountered is re-thrown.
  --
  -- @since 0.1
  removePathForcibly :: (HasCallStack) => OsPath -> m ()

  -- | @'renameDirectory' old new@ changes the name of an existing
  -- directory from /old/ to /new/. If the /new/ directory
  -- already exists, it is atomically replaced by the /old/ directory.
  -- If the /new/ directory is neither the /old/ directory nor an
  -- alias of the /old/ directory, it is removed as if by
  -- 'removeDirectory'. A conformant implementation need not support
  -- renaming directories in all situations (e.g. renaming to an existing
  -- directory, or across different physical devices), but the constraints
  -- must be documented.
  --
  -- On Win32 platforms, @renameDirectory@ fails if the /new/ directory already
  -- exists.
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- Either operand is not a valid directory name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The original directory does not exist, or there is no path to the target.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES, EPERM]@
  --
  -- * 'System.IO.isFullError'
  -- Insufficient resources are available to perform the operation.
  -- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
  --
  -- * @UnsatisfiedConstraints@
  -- Implementation-dependent constraints are not satisfied.
  -- @[EBUSY, ENOTEMPTY, EEXIST]@
  --
  -- * @UnsupportedOperation@
  -- The implementation does not support renaming in this situation.
  -- @[EINVAL, EXDEV]@
  --
  -- * @InappropriateType@
  -- Either path refers to an existing non-directory object.
  -- @[ENOTDIR, EISDIR]@
  --
  -- @since 0.1
  renameDirectory :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Change the working directory to the given path.
  --
  -- In a multithreaded program, the current working directory is a global state
  -- shared among all threads of the process. Therefore, when performing
  -- filesystem operations from multiple threads, it is highly recommended to
  -- use absolute rather than relative paths (see: 'makeAbsolute').
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- The operand is not a valid directory name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The directory does not exist.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EACCES]@
  --
  -- * @UnsupportedOperation@
  -- The operating system has no notion of current working directory, or the
  -- working directory cannot be dynamically changed.
  --
  -- * @InappropriateType@
  -- The path refers to an existing non-directory object.
  -- @[ENOTDIR]@
  --
  -- @since 0.1
  setCurrentDirectory :: (HasCallStack) => OsPath -> m ()

  -- | Run an @m@ action with the given working directory and restore the
  -- original working directory afterwards, even if the given action fails due
  -- to an exception.
  --
  -- The operation may fail with the same exceptions as 'getCurrentDirectory'
  -- and 'setCurrentDirectory'.
  --
  -- @since 0.1
  withCurrentDirectory :: (HasCallStack) => OsPath -> m a -> m a

  -- | 'removeFile' /file/ removes the directory entry for an existing file
  -- /file/, where /file/ is not itself a directory. The
  -- implementation may specify additional constraints which must be
  -- satisfied before a file can be removed (e.g. the file may not be in
  -- use by other processes).
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- The operand is not a valid file name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The file does not exist.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES, EPERM]@
  --
  -- * @UnsatisfiedConstraints@
  -- Implementation-dependent constraints are not satisfied.
  -- @[EBUSY]@
  --
  -- * @InappropriateType@
  -- The operand refers to an existing directory.
  -- @[EPERM, EINVAL]@
  --
  -- @since 0.1
  removeFile :: (HasCallStack) => OsPath -> m ()

  -- | @'renameFile' old new@ changes the name of an existing file system
  -- object from /old/ to /new/. If the /new/ object already exists, it is
  -- replaced by the /old/ object. Neither path may refer to an existing
  -- directory. A conformant implementation need not support renaming files
  -- in all situations (e.g. renaming across different physical devices), but
  -- the constraints must be documented.
  --
  -- On Windows, this calls @MoveFileEx@ with @MOVEFILE_REPLACE_EXISTING@ set,
  -- which is not guaranteed to be atomic
  -- (<https://github.com/haskell/directory/issues/109>).
  --
  -- On other platforms, this operation is atomic.
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- Either operand is not a valid file name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The original file does not exist, or there is no path to the target.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES, EPERM]@
  --
  -- * 'System.IO.isFullError'
  -- Insufficient resources are available to perform the operation.
  -- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
  --
  -- * @UnsatisfiedConstraints@
  -- Implementation-dependent constraints are not satisfied.
  -- @[EBUSY]@
  --
  -- * @UnsupportedOperation@
  -- The implementation does not support renaming in this situation.
  -- @[EXDEV]@
  --
  -- * @InappropriateType@
  -- Either path refers to an existing directory.
  -- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
  --
  --
  -- @since 0.1
  renameFile :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Rename a file or directory. If the destination path already exists, it
  -- is replaced atomically. The destination path must not point to an existing
  -- directory. A conformant implementation need not support renaming files in
  -- all situations (e.g. renaming across different physical devices), but the
  -- constraints must be documented.
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * @InvalidArgument@
  -- Either operand is not a valid file name.
  -- @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  -- The original file does not exist, or there is no path to the target.
  -- @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EROFS, EACCES, EPERM]@
  --
  -- * 'System.IO.isFullError'
  -- Insufficient resources are available to perform the operation.
  -- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
  --
  -- * @UnsatisfiedConstraints@
  -- Implementation-dependent constraints are not satisfied.
  -- @[EBUSY]@
  --
  -- * @UnsupportedOperation@
  -- The implementation does not support renaming in this situation.
  -- @[EXDEV]@
  --
  -- * @InappropriateType@
  -- Either the destination path refers to an existing directory, or one of the
  -- parent segments in the destination path is not a directory.
  -- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
  --
  -- @since 0.1
  renamePath ::
    (HasCallStack) =>
    -- | Old path
    OsPath ->
    -- | New path
    OsPath ->
    m ()

  -- | Copy a file with its permissions. If the destination file already exists,
  -- it is replaced atomically. Neither path may refer to an existing
  -- directory. No exceptions are thrown if the permissions could not be
  -- copied.
  --
  -- @since 0.1
  copyFile ::
    (HasCallStack) =>
    -- | Source filename
    OsPath ->
    -- | Destination filename
    OsPath ->
    m ()

  -- | Copy a file with its associated metadata. If the destination file
  -- already exists, it is overwritten. There is no guarantee of atomicity in
  -- the replacement of the destination file. Neither path may refer to an
  -- existing directory. If the source and/or destination are symbolic links,
  -- the copy is performed on the targets of the links.
  --
  -- On Windows, it behaves like the Win32 function
  -- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa363851.aspx CopyFile>,
  -- which copies various kinds of metadata including file attributes and
  -- security resource properties.
  --
  -- On Unix-like systems, permissions, access time, and modification time are
  -- preserved. If possible, the owner and group are also preserved. Note that
  -- the very act of copying can change the access time of the source file,
  -- hence the access times of the two files may differ after the operation
  -- completes.
  --
  -- @since 0.1
  copyFileWithMetadata ::
    (HasCallStack) =>
    -- | Source file
    OsPath ->
    -- | Destination file
    OsPath ->
    m ()

  -- | Create a /file/ symbolic link. The target path can be either absolute or
  -- relative and need not refer to an existing file. The order of arguments
  -- follows the POSIX convention.
  --
  -- To remove an existing file symbolic link, use 'removeFile'.
  --
  -- Although the distinction between /file/ symbolic links and /directory/
  -- symbolic links does not exist on POSIX systems, on Windows this is an
  -- intrinsic property of every symbolic link and cannot be changed without
  -- recreating the link. A file symbolic link that actually points to a
  -- directory will fail to dereference and vice versa. Moreover, creating
  -- symbolic links on Windows may require privileges unavailable to users
  -- outside the Administrators group. Portable programs that use symbolic
  -- links should take both into consideration.
  --
  -- On Windows, the function is implemented using @CreateSymbolicLink@. Since
  -- 1.3.3.0, the @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is included
  -- if supported by the operating system. On POSIX, the function uses @symlink@
  -- and is therefore atomic.
  --
  -- Windows-specific errors: This operation may fail with 'permissionErrorType'
  -- if the user lacks the privileges to create symbolic links. It may also
  -- fail with 'illegalOperationErrorType' if the file system does not support
  -- symbolic links.
  --
  -- @since 0.1
  createFileLink ::
    (HasCallStack) =>
    -- | path to the target file
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Create a /directory/ symbolic link. The target path can be either
  -- absolute or relative and need not refer to an existing directory. The
  -- order of arguments follows the POSIX convention.
  --
  -- To remove an existing directory symbolic link, use 'removeDirectoryLink'.
  --
  -- Although the distinction between /file/ symbolic links and /directory/
  -- symbolic links does not exist on POSIX systems, on Windows this is an
  -- intrinsic property of every symbolic link and cannot be changed without
  -- recreating the link. A file symbolic link that actually points to a
  -- directory will fail to dereference and vice versa. Moreover, creating
  -- symbolic links on Windows may require privileges unavailable to users
  -- outside the Administrators group. Portable programs that use symbolic
  -- links should take both into consideration.
  --
  -- On Windows, the function is implemented using @CreateSymbolicLink@ with
  -- @SYMBOLIC_LINK_FLAG_DIRECTORY@. Since 1.3.3.0, the
  -- @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is also included if
  -- supported by the operating system. On POSIX, this is an alias for
  -- 'createFileLink' and is therefore atomic.
  --
  -- Windows-specific errors: This operation may fail with 'permissionErrorType'
  -- if the user lacks the privileges to create symbolic links. It may also
  -- fail with 'illegalOperationErrorType' if the file system does not support
  -- symbolic links.
  --
  -- @since 0.1
  createDirectoryLink ::
    (HasCallStack) =>
    -- | path to the target directory
    OsPath ->
    -- | path of the link to be created
    OsPath ->
    m ()

  -- | Remove an existing /directory/ symbolic link.
  --
  -- On Windows, this is an alias for 'removeDirectory'. On POSIX systems, this
  -- is an alias for 'removeFile'.
  --
  -- See also: 'removeFile', which can remove an existing /file/ symbolic link.
  --
  -- @since 0.1
  removeDirectoryLink :: (HasCallStack) => OsPath -> m ()

  -- | Remove an existing /directory/ symbolic link.
  --
  -- On Windows, this is an alias for 'removeDirectory'. On POSIX systems, this
  -- is an alias for 'removeFile'.
  --
  -- See also: 'removeFile', which can remove an existing /file/ symbolic link.
  --
  -- @since 0.1
  setPermissions :: (HasCallStack) => OsPath -> Permissions -> m ()

  -- | Copy the permissions of one file to another. This reproduces the
  -- permissions more accurately than using 'getPermissions' followed by
  -- 'setPermissions'.
  --
  -- On Windows, this copies only the read-only attribute.
  --
  -- On POSIX systems, this is equivalent to @stat@ followed by @chmod@.
  --
  -- @since 0.1
  copyPermissions :: (HasCallStack) => OsPath -> OsPath -> m ()

  -- | Change the time at which the file or directory was last accessed.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError' if the user is not permitted to alter the
  --   access time; or
  --
  -- * 'isDoesNotExistError' if the file or directory does not exist.
  --
  -- Some caveats for POSIX systems:
  --
  -- * Not all systems support @utimensat@, in which case the function can only
  --   emulate the behavior by reading the modification time and then setting
  --   both the access and modification times together. On systems where
  --   @utimensat@ is supported, the access time is set atomically with
  --   nanosecond precision.
  --
  -- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
  --   would not be able to set timestamps with sub-second resolution. In this
  --   case, there would also be loss of precision in the modification time.
  --
  -- @since 0.1
  setAccessTime :: (HasCallStack) => OsPath -> UTCTime -> m ()

  -- | Change the time at which the file or directory was last modified.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError' if the user is not permitted to alter the
  --   modification time; or
  --
  -- * 'isDoesNotExistError' if the file or directory does not exist.
  --
  -- Some caveats for POSIX systems:
  --
  -- * Not all systems support @utimensat@, in which case the function can only
  --   emulate the behavior by reading the access time and then setting both the
  --   access and modification times together. On systems where @utimensat@ is
  --   supported, the modification time is set atomically with nanosecond
  --   precision.
  --
  -- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
  --   would not be able to set timestamps with sub-second resolution. In this
  --   case, there would also be loss of precision in the access time.
  --
  -- @since 0.1
  setModificationTime :: (HasCallStack) => OsPath -> UTCTime -> m ()

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

-- | Exception for trying to create a path that already exists.
--
-- @since 0.1
newtype PathExistsException = MkPathExistsException OsPath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathExistsException where
  displayException (MkPathExistsException path) =
    "Path already exists: " <> pathToStr path

-- | Exception for when a path does not exist.
--
-- @since 0.1
newtype PathDoesNotExistException = MkPathDoesNotExistException OsPath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathDoesNotExistException where
  displayException (MkPathDoesNotExistException path) =
    "Path does not exist: " <> pathToStr path

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
    (\() -> OverwriteNone)
    ( \x -> case x of
        OverwriteNone -> Right ()
        _ -> Left x
    )
{-# INLINE _OverwriteNone #-}

-- | @since 0.1
_OverwriteDirectories :: Prism' Overwrite ()
_OverwriteDirectories =
  prism
    (\() -> OverwriteDirectories)
    ( \x -> case x of
        OverwriteDirectories -> Right ()
        _ -> Left x
    )
{-# INLINE _OverwriteDirectories #-}

-- | @since 0.1
_OverwriteAll :: Prism' Overwrite ()
_OverwriteAll =
  prism
    (\() -> OverwriteAll)
    ( \x -> case x of
        OverwriteAll -> Right ()
        _ -> Left x
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
    (\() -> TargetNameSrc)
    ( \x -> case x of
        TargetNameSrc -> Right ()
        _ -> Left x
    )
{-# INLINE _TargetNameSrc #-}

-- | @since 0.1
_TargetNameLiteral :: Prism' TargetName OsPath
_TargetNameLiteral =
  prism
    TargetNameLiteral
    ( \x -> case x of
        TargetNameLiteral p -> Right p
        _ -> Left x
    )
{-# INLINE _TargetNameLiteral #-}

-- | @since 0.1
_TargetNameDest :: Prism' TargetName ()
_TargetNameDest =
  prism
    (\() -> TargetNameDest)
    ( \x -> case x of
        TargetNameDest -> Right ()
        _ -> Left x
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
  labelOptic = lensVL $ \f (MkCopyDirConfig _overwrite _targetName) ->
    fmap (`MkCopyDirConfig` _targetName) (f _overwrite)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TargetName, b ~ TargetName) =>
  LabelOptic "targetName" k CopyDirConfig CopyDirConfig a b
  where
  labelOptic = lensVL $ \f (MkCopyDirConfig _overwrite _targetName) ->
    fmap (MkCopyDirConfig _overwrite) (f _targetName)
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
-- * 'PathDoesNotExistException': if @dest@ does not exist.
-- * 'PathExistsException':
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
  destExists <- doesDirectoryExist destRoot

  unless destExists $
    throwCS $
      MkPathDoesNotExistException destRoot

  -- NOTE: Use the given name if it exists. Otherwise use the source folder's
  -- name.
  let dest = case config ^. #targetName of
        -- Use source directory's name
        TargetNameSrc ->
          destRoot </> FP.takeBaseName (FP.dropTrailingPathSeparator src)
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

  destExists <- doesDirectoryExist dest

  let checkOverwrites =
        if not overwriteFiles
          then \f -> do
            exists <- doesFileExist f
            when exists $
              throwCS $
                MkPathExistsException f
          else const (pure ())

      copyFiles = do
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
          let f' = dest </> f
          checkOverwrites f'
          copyFileWithMetadata (src </> f) f'
          modifyIORef' copiedFilesRef (f' :)

      cleanup =
        if destExists
          then do
            -- manually delete files and dirs
            readIORef copiedFilesRef >>= traverse_ removeFile
            readIORef createdDirsRef >>= traverse_ removeDirectory
          else removeDirectoryRecursive dest

  copyFiles `onException` mask_ cleanup

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
  when destExists $ throwCS (MkPathExistsException dest)

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

removeIfExists :: (Monad m) => (t -> m Bool) -> (t -> m ()) -> t -> m ()
removeIfExists existsFn deleteFn f =
  existsFn f >>= \b -> when b (deleteFn f)
{-# INLINEABLE removeIfExists #-}

pathToStr :: OsPath -> String
pathToStr = fmap FP.toChar . FP.unpack
