{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effects.FileSystem.IO
  ( -- * File handling
    readBinaryFileIO,
    writeBinaryFileIO,
    appendBinaryFileIO,
    openBinaryFileIO,
    withBinaryFileIO,

    -- * Errors
    throwPathIOError,
    throwPathIOErrorFilePath,
  )
where

import Data.ByteString (ByteString)
import Effects.Exception (MonadThrow, throwM)
import Effects.FileSystem.OsPath qualified as FS.OsPath
import GHC.IO.Exception
  ( IOException
      ( IOError,
        ioe_description,
        ioe_errno,
        ioe_filename,
        ioe_handle,
        ioe_location,
        ioe_type
      ),
  )
import GHC.Stack.Types (HasCallStack)
import System.File.OsPath qualified as FileIO
import System.IO (Handle, IOMode)
import System.IO.Error (IOErrorType)
import System.OsPath (OsPath)

-- | @since 0.1
readBinaryFileIO :: OsPath -> IO ByteString
readBinaryFileIO = FileIO.readFile'

-- | @since 0.1
writeBinaryFileIO :: OsPath -> ByteString -> IO ()
writeBinaryFileIO = FileIO.writeFile'

-- | @since 0.1
appendBinaryFileIO :: OsPath -> ByteString -> IO ()
appendBinaryFileIO = FileIO.appendFile'

-- | @since 0.1
openBinaryFileIO :: OsPath -> IOMode -> IO Handle
openBinaryFileIO = FileIO.openBinaryFile

-- | @since 0.1
withBinaryFileIO :: OsPath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO = FileIO.withBinaryFile

-- | Helper for throwing 'IOException'.
--
-- @since 0.1
throwPathIOError ::
  (HasCallStack, MonadThrow m) =>
  -- | Path upon which the IO operation failed.
  OsPath ->
  -- | String location (e.g. function name).
  String ->
  -- | Type of exception.
  IOErrorType ->
  -- | Description.
  String ->
  m a
throwPathIOError = throwPathIOErrorFilePath . FS.OsPath.decodeLenient

-- | Helper for throwing 'IOException' with legacy FilePath.
--
-- @since 0.1
throwPathIOErrorFilePath ::
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
throwPathIOErrorFilePath path loc ty desc =
  throwM $
    IOError
      { ioe_handle = Nothing,
        ioe_type = ty,
        ioe_location = loc,
        ioe_description = desc,
        ioe_errno = Nothing,
        ioe_filename = Just path
      }
{-# INLINEABLE throwPathIOErrorFilePath #-}
