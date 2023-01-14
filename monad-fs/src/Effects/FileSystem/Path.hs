{-# LANGUAGE CPP #-}

-- | Provides compatibility for filepaths.
--
-- @since 0.1
module Effects.FileSystem.Path
  ( -- * Path
    Path,
    (</>),

    -- * System.IO
    openBinaryFileIO,
    withBinaryFileIO,

    -- * Data.ByteString
    readBinaryFileIO,
    writeBinaryFileIO,
    appendBinaryFileIO,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import Prelude hiding (FilePath)

#if MIN_VERSION_filepath(1,4,100) && MIN_VERSION_directory(1,3,8)
import Control.Monad ((>=>))
import System.OsPath (OsPath, (</>))

-- | For @filepath >= 1.4.100@ and @directory >= 1.3.8@, 'Path' = 'OsPath'.
-- Below that it is a 'FilePath'.
--
-- @since 0.1
type Path = OsPath

-- | @since 0.1
readBinaryFileIO :: OsPath -> IO ByteString
readBinaryFileIO = decodeFS >=> BS.readFile

-- | @since 0.1
writeBinaryFileIO :: OsPath -> ByteString -> IO ()
writeBinaryFileIO = decodeFS >=> BS.writeFile

-- | @since 0.1
appendBinaryFileIO :: OsPath -> ByteString -> IO ()
appendBinaryFileIO = decodeFS >=> BS.appendFile

-- | @since 0.1
openBinaryFileIO :: OsPath -> IOMode -> IO Handle
openBinaryFileIO p m = decodeFS p >>= \h -> IO.openBinaryFile h m

-- | @since 0.1
withBinaryFileIO :: OsPath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO p m f = decodeFS p >>= \h -> IO.withBinaryFile h m f

#else
import System.FilePath (FilePath, (</>))

-- | For @filepath >= 1.4.100@ and @directory >= 1.3.8@,
-- 'Path' = @System.FilePath.OsPath@. Below that it is a 'FilePath'.
--
-- @since 0.1
type Path = FilePath

-- | @since 0.1
readBinaryFileIO :: FilePath -> IO ByteString
readBinaryFileIO = BS.readFile

-- | @since 0.1
writeBinaryFileIO :: FilePath -> ByteString -> IO ()
writeBinaryFileIO = BS.writeFile

-- | @since 0.1
appendBinaryFileIO :: FilePath -> ByteString -> IO ()
appendBinaryFileIO = BS.appendFile

-- | @since 0.1
openBinaryFileIO :: FilePath -> IOMode -> IO Handle
openBinaryFileIO = IO.openBinaryFile

-- | @since 0.1
withBinaryFileIO :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO = IO.withBinaryFile
#endif
