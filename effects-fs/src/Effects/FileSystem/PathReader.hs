{-# LANGUAGE CPP #-}

{- ORMOLU_DISABLE -}

-- | Provides the MonadPathReader effect.
--
-- @since 0.1
module Effects.FileSystem.PathReader
  ( -- * Effect
    MonadPathReader (..),
    Path,

    -- ** Functions
    findFile,
    findFiles,


    -- * Xdg Utils
    getXdgData,
    getXdgConfig,
    getXdgCache,
#if MIN_VERSION_directory(1,3,7)
    getXdgState,
#endif

    -- * Misc
    listDirectoryRecursive,

    -- * Reexports
    XdgDirectory (..),
    XdgDirectoryList (..),
    Permissions (..),
    UTCTime (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Time (UTCTime (..))
import Effects.Exception (addCS)
import Effects.FileSystem.Path (Path, (</>))
import GHC.Stack (HasCallStack)
import System.Directory
  ( Permissions (..),
    XdgDirectory (..),
    XdgDirectoryList (..),
  )
#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import System.Directory.OsPath qualified as Dir
#else
import System.Directory qualified as Dir
#endif

{- ORMOLU_ENABLE -}

-- REVIEW: Can we reduce the class size by implementing some of these
-- functions in terms of others?

-- | Represents file-system reader effects.
--
-- @since 0.1
class (Monad m) => MonadPathReader m where
  -- | @'listDirectory' dir@ returns a list of /all/ entries in /dir/ without
  -- the special entries (@.@ and @..@).
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  --   A physical I\/O error has occurred.
  --   @[EIO]@
  --
  -- * @InvalidArgument@
  --   The operand is not a valid directory name.
  --   @[ENAMETOOLONG, ELOOP]@
  --
  -- * 'isDoesNotExistError'
  --   The directory does not exist.
  --   @[ENOENT, ENOTDIR]@
  --
  -- * 'isPermissionError'
  --   The process has insufficient privileges to perform the operation.
  --   @[EACCES]@
  --
  -- * 'System.IO.isFullError'
  --   Insufficient resources are available to perform the operation.
  --   @[EMFILE, ENFILE]@
  --
  -- * @InappropriateType@
  --   The path refers to an existing non-directory object.
  --   @[ENOTDIR]@
  --
  -- @since 0.1
  listDirectory :: (HasCallStack) => Path -> m [Path]

  -- | Similar to 'listDirectory', but always includes the special entries (@.@
  -- and @..@). (This applies to Windows as well.)
  --
  -- The operation may fail with the same exceptions as 'listDirectory'.
  --
  -- @since 0.1
  getDirectoryContents :: (HasCallStack) => Path -> m [Path]

  -- | Obtain the current working directory as an absolute path.
  --
  -- In a multithreaded program, the current working directory is a global state
  -- shared among all threads of the process. Therefore, when performing
  -- filesystem operations from multiple threads, it is highly recommended to
  -- use absolute rather than relative paths (see: 'makeAbsolute').
  --
  -- Note that 'getCurrentDirectory' is not guaranteed to return the same path
  -- received by 'setCurrentDirectory'. On POSIX systems, the path returned will
  -- always be fully dereferenced (not contain any symbolic links). For more
  -- information, refer to the documentation of
  -- <https://pubs.opengroup.org/onlinepubs/9699919799/functions/getcwd.html getcwd>.
  --
  -- The operation may fail with:
  --
  -- * @HardwareFault@
  -- A physical I\/O error has occurred.
  -- @[EIO]@
  --
  -- * 'isDoesNotExistError'
  -- There is no path referring to the working directory.
  -- @[EPERM, ENOENT, ESTALE...]@
  --
  -- * 'isPermissionError'
  -- The process has insufficient privileges to perform the operation.
  -- @[EACCES]@
  --
  -- * 'System.IO.isFullError'
  -- Insufficient resources are available to perform the operation.
  --
  -- * @UnsupportedOperation@
  -- The operating system has no notion of current working directory.
  --
  -- @since 0.1
  getCurrentDirectory :: (HasCallStack) => m Path

  -- | Returns the current user's home directory.
  --
  -- The directory returned is expected to be writable by the current user,
  -- but note that it isn't generally considered good practice to store
  -- application-specific data here; use 'getXdgDirectory' or
  -- 'getAppUserDataDirectory' instead.
  --
  -- On Unix, 'getHomeDirectory' behaves as follows:
  --
  -- * Returns $HOME env variable if set (including to an empty string).
  -- * Otherwise uses home directory returned by `getpwuid_r` using the UID of
  -- the current proccesses user. This basically reads the /etc/passwd file.
  -- An empty home directory field is considered valid.
  --
  -- On Windows, the system is queried for a suitable path; a typical path
  -- might be @C:\/Users\//\<user\>/@.
  --
  -- The operation may fail with:
  --
  -- * @UnsupportedOperation@
  -- The operating system has no notion of home directory.
  --
  -- * 'isDoesNotExistError'
  -- The home directory for the current user does not exist, or
  -- cannot be found.
  --
  -- @since 0.1
  getHomeDirectory :: (HasCallStack) => m Path

  -- | Obtain the paths to special directories for storing user-specific
  -- application data, configuration, and cache files, conforming to the
  -- <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
  -- XDG Base Directory Specification>.
  -- Compared with 'getAppUserDataDirectory', this function provides a more
  -- fine-grained hierarchy as well as greater flexibility for the user.
  --
  -- On Windows, 'XdgData' and 'XdgConfig' usually map to the same directory
  -- unless overridden.
  --
  -- Refer to the docs of 'XdgDirectory' for more details.
  --
  -- The second argument is usually the name of the application. Since it
  -- will be integrated into the path, it must consist of valid path
  -- characters. Note: if the second argument is an absolute path, it will
  -- just return the second argument.
  --
  -- Note: The directory may not actually exist, in which case you would need
  -- to create it with file mode @700@ (i.e. only accessible by the owner).
  --
  -- The environment variable is ignored if set to a relative
  -- path, per revised XDG Base Directory Specification. See
  -- <https://github.com/haskell/directory/issues/100 #100>.
  --
  -- @since 0.1
  getXdgDirectory :: (HasCallStack) => XdgDirectory -> Path -> m Path

  -- | Similar to 'getXdgDirectory' but retrieves the entire list of XDG
  -- directories.
  --
  -- On Windows, 'XdgDataDirs' and 'XdgConfigDirs' usually map to the same list
  -- of directories unless overridden.
  --
  -- Refer to the docs of 'XdgDirectoryList' for more details.
  --
  -- @since 0.1
  getXdgDirectoryList :: (HasCallStack) => XdgDirectoryList -> m [Path]

  -- | Obtain the path to a special directory for storing user-specific
  -- application data (traditional Unix location). Newer applications may
  -- prefer the the XDG-conformant location provided by 'getXdgDirectory'
  -- (<https://github.com/haskell/directory/issues/6#issuecomment-96521020 migration guide>).
  --
  -- The argument is usually the name of the application. Since it will be
  -- integrated into the path, it must consist of valid path characters.
  --
  -- * On Unix-like systems, the path is @~\/./\<app\>/@.
  -- * On Windows, the path is @%APPDATA%\//\<app\>/@
  --   (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming\//\<app\>/@)
  --
  -- Note: the directory may not actually exist, in which case you would need
  -- to create it. It is expected that the parent directory exists and is
  -- writable.
  --
  -- The operation may fail with:
  --
  -- * @UnsupportedOperation@
  --   The operating system has no notion of application-specific data
  --   directory.
  --
  -- * 'isDoesNotExistError'
  --   The home directory for the current user does not exist, or cannot be
  --   found.
  --
  -- @since 0.1
  getAppUserDataDirectory :: (HasCallStack) => Path -> m Path

  -- | Returns the current user's document directory.
  --
  -- The directory returned is expected to be writable by the current user,
  -- but note that it isn't generally considered good practice to store
  -- application-specific data here; use 'getXdgDirectory' or
  -- 'getAppUserDataDirectory' instead.
  --
  -- On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
  -- environment variable. On Windows, the system is queried for a
  -- suitable path; a typical path might be @C:\/Users\//\<user\>/\/Documents@.
  --
  -- The operation may fail with:
  --
  -- * @UnsupportedOperation@
  -- The operating system has no notion of document directory.
  --
  -- * 'isDoesNotExistError'
  -- The document directory for the current user does not exist, or
  -- cannot be found.
  --
  -- @since 0.1
  getUserDocumentsDirectory :: (HasCallStack) => m Path

  -- | Returns the current directory for temporary files.
  --
  -- On Unix, 'getTemporaryDirectory' returns the value of the @TMPDIR@
  -- environment variable or \"\/tmp\" if the variable isn\'t defined.
  -- On Windows, the function checks for the existence of environment variables in
  -- the following order and uses the first path found:
  --
  -- *
  -- TMP environment variable.
  --
  -- *
  -- TEMP environment variable.
  --
  -- *
  -- USERPROFILE environment variable.
  --
  -- *
  -- The Windows directory
  --
  -- The operation may fail with:
  --
  -- * @UnsupportedOperation@
  -- The operating system has no notion of temporary directory.
  --
  -- The function doesn\'t verify whether the path exists.
  --
  -- @since 0.1
  getTemporaryDirectory :: (HasCallStack) => m Path

  -- | Obtain the size of a file in bytes.
  --
  -- @since 0.1
  getFileSize :: (HasCallStack) => Path -> m Integer

  -- | Make a path absolute, normalize the path, and remove as many indirections
  -- from it as possible. Any trailing path separators are discarded via
  -- 'dropTrailingPathSeparator'. Additionally, on Windows the letter case of
  -- the path is canonicalized.
  --
  -- __Note__: This function is a very big hammer. If you only need an absolute
  -- path, 'makeAbsolute' is sufficient for removing dependence on the current
  -- working directory.
  --
  -- Indirections include the two special directories @.@ and @..@, as well as
  -- any symbolic links (and junction points on Windows). The input path need
  -- not point to an existing file or directory. Canonicalization is performed
  -- on the longest prefix of the path that points to an existing file or
  -- directory. The remaining portion of the path that does not point to an
  -- existing file or directory will still be normalized, but case
  -- canonicalization and indirection removal are skipped as they are impossible
  -- to do on a nonexistent path.
  --
  -- Most programs should not worry about the canonicity of a path. In
  -- particular, despite the name, the function does not truly guarantee
  -- canonicity of the returned path due to the presence of hard links, mount
  -- points, etc.
  --
  -- If the path points to an existing file or directory, then the output path
  -- shall also point to the same file or directory, subject to the condition
  -- that the relevant parts of the file system do not change while the function
  -- is still running. In other words, the function is definitively not atomic.
  -- The results can be utterly wrong if the portions of the path change while
  -- this function is running.
  --
  -- Since some indirections (symbolic links on all systems, @..@ on non-Windows
  -- systems, and junction points on Windows) are dependent on the state of the
  -- existing filesystem, the function can only make a conservative attempt by
  -- removing such indirections from the longest prefix of the path that still
  -- points to an existing file or directory.
  --
  -- Note that on Windows parent directories @..@ are always fully expanded
  -- before the symbolic links, as consistent with the rest of the Windows API
  -- (such as @GetFullPathName@). In contrast, on POSIX systems parent
  -- directories @..@ are expanded alongside symbolic links from left to right.
  -- To put this more concretely: if @L@ is a symbolic link for @R/P@, then on
  -- Windows @L\\..@ refers to @.@, whereas on other operating systems @L/..@
  -- refers to @R@.
  --
  -- Similar to 'System.FilePath.normalise', passing an empty path is equivalent
  -- to passing the current directory.
  --
  -- @canonicalizePath@ can resolve at least 64 indirections in a single path,
  -- more than what is supported by most operating systems. Therefore, it may
  -- return the fully resolved path even though the operating system itself
  -- would have long given up.
  --
  -- On Windows XP or earlier systems, junction expansion is not performed due
  -- to their lack of @GetFinalPathNameByHandle@.
  --
  -- /Changes since 1.2.3.0:/ The function has been altered to be more robust
  -- and has the same exception behavior as 'makeAbsolute'.
  --
  -- /Changes since 1.3.0.0:/ The function no longer preserves the trailing path
  -- separator. File symbolic links that appear in the middle of a path are
  -- properly dereferenced. Case canonicalization and symbolic link expansion
  -- are now performed on Windows.
  --
  -- @since 0.1
  canonicalizePath :: (HasCallStack) => Path -> m Path

  -- | Convert a path into an absolute path. If the given path is relative, the
  -- current directory is prepended and then the combined result is normalized.
  -- If the path is already absolute, the path is simply normalized. The
  -- function preserves the presence or absence of the trailing path separator
  -- unless the path refers to the root directory @/@.
  --
  -- If the path is already absolute, the operation never fails. Otherwise, the
  -- operation may fail with the same exceptions as 'getCurrentDirectory'.
  --
  -- @since 0.1
  makeAbsolute :: (HasCallStack) => Path -> m Path

  -- | Construct a path relative to the current directory, similar to
  -- 'makeRelative'.
  --
  -- The operation may fail with the same exceptions as 'getCurrentDirectory'.
  --
  -- @since 0.1
  makeRelativeToCurrentDirectory :: (HasCallStack) => Path -> m Path

  -- | Test whether the given path points to an existing filesystem object. If
  -- the user lacks necessary permissions to search the parent directories,
  -- this function may return false even if the file does actually exist.
  --
  -- @since 0.1
  doesPathExist :: (HasCallStack) => Path -> m Bool

  -- | The operation 'doesFileExist' returns 'True'
  -- if the argument file exists and is not a directory, and 'False' otherwise.
  --
  -- @since 0.1
  doesFileExist :: (HasCallStack) => Path -> m Bool

  -- | The operation 'doesDirectoryExist' returns 'True' if the argument file
  -- exists and is either a directory or a symbolic link to a directory,
  -- and 'False' otherwise.
  --
  -- @since 0.1
  doesDirectoryExist :: (HasCallStack) => Path -> m Bool

  -- | Given the name or path of an executable file, 'findExecutable' searches
  -- for such a file in a list of system-defined locations, which generally
  -- includes @PATH@ and possibly more. The full path to the executable is
  -- returned if found. For example, @(findExecutable \"ghc\")@ would normally
  -- give you the path to GHC.
  --
  -- The path returned by @'findExecutable' name@ corresponds to the program
  -- that would be executed by
  -- @<http://hackage.haskell.org/package/process/docs/System-Process.html#v:createProcess createProcess>@
  -- when passed the same string (as a @RawCommand@, not a @ShellCommand@),
  -- provided that @name@ is not a relative path with more than one segment.
  --
  -- On Windows, 'findExecutable' calls the Win32 function
  -- @<https://msdn.microsoft.com/en-us/library/aa365527.aspx SearchPath>@,
  -- which may search other places before checking the directories in the @PATH@
  -- environment variable. Where it actually searches depends on registry
  -- settings, but notably includes the directory containing the current
  -- executable.
  --
  -- On non-Windows platforms, the behavior is equivalent to 'findFileWith'
  -- using the search directories from the @PATH@ environment variable and
  -- testing each file for executable permissions. Details can be found in the
  -- documentation of 'findFileWith'.
  --
  -- @since 0.1
  findExecutable :: (HasCallStack) => Path -> m (Maybe Path)

  -- | Search for executable files in a list of system-defined locations, which
  -- generally includes @PATH@ and possibly more.
  --
  -- On Windows, this /only returns the first occurrence/, if any. Its behavior
  -- is therefore equivalent to 'findExecutable'.
  --
  -- On non-Windows platforms, the behavior is equivalent to
  -- 'findExecutablesInDirectories' using the search directories from the @PATH@
  -- environment variable. Details can.
  --
  -- @since 0.1
  findExecutables :: (HasCallStack) => Path -> m [Path]

  -- | Given a name or path, 'findExecutable' appends the 'exeExtension' to the
  -- query and searches for executable files in the list of given search
  -- directories and returns all occurrences.
  --
  -- The behavior is equivalent to 'findFileWith' using the given search
  -- directories and testing each file for executable permissions. Details can
  -- be found in the documentation of 'findFileWith'.
  --
  -- Unlike other similarly named functions, 'findExecutablesInDirectories' does
  -- not use @SearchPath@ from the Win32 API. The behavior of this function on
  -- Windows is therefore equivalent to those on non-Windows platforms.
  --
  -- @since 0.1
  findExecutablesInDirectories :: (HasCallStack) => [Path] -> Path -> m [Path]

  -- | Search through a given list of directories for a file that has the given
  -- name and satisfies the given predicate and return the path of the first
  -- occurrence. The directories are checked in a left-to-right order.
  --
  -- This is essentially a more performant version of 'findFilesWith' that
  -- always returns the first result, if any. Details can be found in the
  -- documentation of 'findFilesWith'.
  --
  -- @since 0.1
  findFileWith :: (HasCallStack) => (Path -> m Bool) -> [Path] -> Path -> m (Maybe Path)

  -- | @findFilesWith predicate dirs name@ searches through the list of
  -- directories (@dirs@) for files that have the given @name@ and satisfy the
  -- given @predicate@ and returns the paths of those files. The directories
  -- are checked in a left-to-right order and the paths are returned in the same
  -- order.
  --
  -- If the @name@ is a relative path, then for every search directory @dir@,
  -- the function checks whether @dir '</>' name@ exists and satisfies the
  -- predicate. If so, @dir '</>' name@ is returned as one of the results. In
  -- other words, the returned paths can be either relative or absolute
  -- depending on the search directories were used. If there are no search
  -- directories, no results are ever returned.
  --
  -- If the @name@ is an absolute path, then the function will return a single
  -- result if the file exists and satisfies the predicate and no results
  -- otherwise. This is irrespective of what search directories were given.
  --
  -- @since 0.1
  findFilesWith :: (HasCallStack) => (Path -> m Bool) -> [Path] -> Path -> m [Path]

  -- | Check whether an existing @path@ is a symbolic link. If @path@ is a
  -- regular file or directory, 'False' is returned. If @path@ does not exist
  -- or is otherwise inaccessible, an exception is thrown (see below).
  --
  -- On Windows, this checks for @FILE_ATTRIBUTE_REPARSE_POINT@. In addition to
  -- symbolic links, the function also returns true on junction points. On
  -- POSIX systems, this checks for @S_IFLNK@.
  --
  -- The operation may fail with:
  --
  -- * 'isDoesNotExistError' if the symbolic link does not exist; or
  --
  -- * 'isPermissionError' if the user is not permitted to read the symbolic
  --   link.
  --
  -- @since 0.1
  pathIsSymbolicLink :: (HasCallStack) => Path -> m Bool

  -- | Retrieve the target path of either a file or directory symbolic link.
  -- The returned path may not be absolute, may not exist, and may not even be a
  -- valid path.
  --
  -- On Windows systems, this calls @DeviceIoControl@ with
  -- @FSCTL_GET_REPARSE_POINT@. In addition to symbolic links, the function
  -- also works on junction points. On POSIX systems, this calls @readlink@.
  --
  -- Windows-specific errors: This operation may fail with
  -- 'illegalOperationErrorType' if the file system does not support symbolic
  -- links.
  --
  -- @since 0.1
  getSymbolicLinkTarget :: (HasCallStack) => Path -> m Path

  -- | Get the permissions of a file or directory.
  --
  -- On Windows, the 'writable' permission corresponds to the "read-only"
  -- attribute. The 'executable' permission is set if the file extension is of
  -- an executable file type. The 'readable' permission is always set.
  --
  -- On POSIX systems, this returns the result of @access@.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError' if the user is not permitted to access the
  --   permissions, or
  --
  -- * 'isDoesNotExistError' if the file or directory does not exist.
  --
  -- @since 0.1
  getPermissions :: (HasCallStack) => Path -> m Permissions

  -- | Obtain the time at which the file or directory was last accessed.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError' if the user is not permitted to read
  --   the access time; or
  --
  -- * 'isDoesNotExistError' if the file or directory does not exist.
  --
  -- Caveat for POSIX systems: This function returns a timestamp with sub-second
  -- resolution only if this package is compiled against @unix-2.6.0.0@ or later
  -- and the underlying filesystem supports them.
  --
  -- @since 0.1
  getAccessTime :: (HasCallStack) => Path -> m UTCTime

  -- | Obtain the time at which the file or directory was last modified.
  --
  -- The operation may fail with:
  --
  -- * 'isPermissionError' if the user is not permitted to read
  --   the modification time; or
  --
  -- * 'isDoesNotExistError' if the file or directory does not exist.
  --
  -- Caveat for POSIX systems: This function returns a timestamp with sub-second
  -- resolution only if this package is compiled against @unix-2.6.0.0@ or later
  -- and the underlying filesystem supports them.
  --
  -- @since 0.1
  getModificationTime :: (HasCallStack) => Path -> m UTCTime

instance MonadPathReader IO where
  listDirectory = addCS . Dir.listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = addCS . Dir.getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = addCS Dir.getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = addCS Dir.getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = addCS . Dir.getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = addCS . Dir.getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = addCS . Dir.getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = addCS Dir.getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = addCS Dir.getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = addCS . Dir.getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = addCS . Dir.canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = addCS . Dir.makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = addCS . Dir.makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = addCS . Dir.doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = addCS . Dir.doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = addCS . Dir.doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = addCS . Dir.findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = addCS . Dir.findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = addCS . Dir.findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFileWith f ps = addCS . Dir.findFileWith f ps
  {-# INLINEABLE findFileWith #-}
  findFilesWith f ps = addCS . Dir.findFilesWith f ps
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = addCS . Dir.pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = addCS . Dir.getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = addCS . Dir.getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = addCS . Dir.getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = addCS . Dir.getModificationTime
  {-# INLINEABLE getModificationTime #-}

instance (MonadPathReader m) => MonadPathReader (ReaderT env m) where
  listDirectory = lift . listDirectory
  {-# INLINEABLE listDirectory #-}
  getDirectoryContents = lift . getDirectoryContents
  {-# INLINEABLE getDirectoryContents #-}
  getCurrentDirectory = lift getCurrentDirectory
  {-# INLINEABLE getCurrentDirectory #-}
  getHomeDirectory = lift getHomeDirectory
  {-# INLINEABLE getHomeDirectory #-}
  getXdgDirectory d = lift . getXdgDirectory d
  {-# INLINEABLE getXdgDirectory #-}
  getXdgDirectoryList = lift . getXdgDirectoryList
  {-# INLINEABLE getXdgDirectoryList #-}
  getAppUserDataDirectory = lift . getAppUserDataDirectory
  {-# INLINEABLE getAppUserDataDirectory #-}
  getUserDocumentsDirectory = lift getUserDocumentsDirectory
  {-# INLINEABLE getUserDocumentsDirectory #-}
  getTemporaryDirectory = lift getTemporaryDirectory
  {-# INLINEABLE getTemporaryDirectory #-}
  getFileSize = lift . getFileSize
  {-# INLINEABLE getFileSize #-}
  canonicalizePath = lift . canonicalizePath
  {-# INLINEABLE canonicalizePath #-}
  makeAbsolute = lift . makeAbsolute
  {-# INLINEABLE makeAbsolute #-}
  makeRelativeToCurrentDirectory = lift . makeRelativeToCurrentDirectory
  {-# INLINEABLE makeRelativeToCurrentDirectory #-}
  doesPathExist = lift . doesPathExist
  {-# INLINEABLE doesPathExist #-}
  doesFileExist = lift . doesFileExist
  {-# INLINEABLE doesFileExist #-}
  doesDirectoryExist = lift . doesDirectoryExist
  {-# INLINEABLE doesDirectoryExist #-}
  findExecutable = lift . findExecutable
  {-# INLINEABLE findExecutable #-}
  findExecutables = lift . findExecutables
  {-# INLINEABLE findExecutables #-}
  findExecutablesInDirectories ps = lift . findExecutablesInDirectories ps
  {-# INLINEABLE findExecutablesInDirectories #-}
  findFileWith action ps fileName =
    ask >>= lift . \e -> findFileWith ((`runReaderT` e) . action) ps fileName
  {-# INLINEABLE findFileWith #-}
  findFilesWith action ps fileName =
    ask >>= lift . \e -> findFilesWith ((`runReaderT` e) . action) ps fileName
  {-# INLINEABLE findFilesWith #-}
  pathIsSymbolicLink = lift . pathIsSymbolicLink
  {-# INLINEABLE pathIsSymbolicLink #-}
  getSymbolicLinkTarget = lift . getSymbolicLinkTarget
  {-# INLINEABLE getSymbolicLinkTarget #-}
  getPermissions = lift . getPermissions
  {-# INLINEABLE getPermissions #-}
  getAccessTime = lift . getAccessTime
  {-# INLINEABLE getAccessTime #-}
  getModificationTime = lift . getModificationTime
  {-# INLINEABLE getModificationTime #-}

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence. Details can be found in the documentation of 'findFileWith'.
--
-- @since 0.1
findFile :: (HasCallStack, MonadPathReader m) => [Path] -> Path -> m (Maybe Path)
findFile = findFileWith (\_ -> pure True)
{-# INLINEABLE findFile #-}

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'. Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 0.1
findFiles :: (HasCallStack, MonadPathReader m) => [Path] -> Path -> m [Path]
findFiles = findFilesWith (\_ -> pure True)
{-# INLINEABLE findFiles #-}

-- | Retrieves the Xdg data directory e.g. @~/.local\/share@.
--
-- @since 0.1
getXdgData :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgData = getXdgDirectory XdgData
{-# INLINEABLE getXdgData #-}

-- | Retrieves the Xdg config directory e.g. @~/.config@.
--
-- @since 0.1
getXdgConfig :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgConfig = getXdgDirectory XdgConfig
{-# INLINEABLE getXdgConfig #-}

-- | Retrieves the Xdg cache directory e.g. @~/.cache@.
--
-- @since 0.1
getXdgCache :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgCache = getXdgDirectory XdgCache
{-# INLINEABLE getXdgCache #-}

#if MIN_VERSION_directory(1,3,7)
-- | Retrieves the Xdg state directory e.g. @~/.local\/state@.
--
-- @since 0.1
getXdgState :: (HasCallStack, MonadPathReader m) => Path -> m Path
getXdgState = getXdgDirectory XdgState
{-# INLINEABLE getXdgState #-}
#endif

-- | Retrieves the recursive directory contents; splits the sub folders and
-- directories apart.
--
-- @since 0.1
listDirectoryRecursive ::
  forall m.
  ( HasCallStack,
    MonadPathReader m
  ) =>
  -- | Root path.
  Path ->
  -- | (files, directories)
  m ([Path], [Path])
listDirectoryRecursive root = recurseDirs [""]
  where
    recurseDirs :: [Path] -> m ([Path], [Path])
    recurseDirs [] = pure ([], [])
    recurseDirs (d : ds) = do
      (files, dirs) <- splitPaths root d [] [] =<< listDirectory (root </> d)
      (files', dirs') <- recurseDirs (dirs ++ ds)
      pure (files ++ files', dirs ++ dirs')

splitPaths ::
  forall m.
  ( HasCallStack,
    MonadPathReader m
  ) =>
  Path ->
  Path ->
  [Path] ->
  [Path] ->
  [Path] ->
  m ([Path], [Path])
splitPaths root d = go
  where
    go :: [Path] -> [Path] -> [Path] -> m ([Path], [Path])
    go files dirs [] = pure (reverse files, reverse dirs)
    go files dirs (p : ps) = do
      let dirEntry = d </> p
      isDir <- doesDirectoryExist (root </> dirEntry)
      if isDir
        then go files (dirEntry : dirs) ps
        else go (dirEntry : files) dirs ps
