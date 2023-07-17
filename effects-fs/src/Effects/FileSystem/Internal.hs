-- | Internal module.
--
-- @since 0.1
module Effects.FileSystem.Internal
  ( -- * Path
    OsPath,
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

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.IO (Handle, IOMode)
import System.IO qualified as IO
import System.OsPath (OsPath, decodeUtf, (</>))
import Prelude hiding (FilePath)

-- NOTE: decodeUtf vs. decodeFs
--
-- The latter (decodeFs) is closer to what base used to do, so using it
-- would most closely keep the previous semantics. So why do we use decodeUtf
-- instead? Because the latter relies on the environment locale and seems
-- more likely to cause strange errors. See the haddocks and also the
-- following blog post.
--
-- https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html

-- | @since 0.1
readBinaryFileIO :: OsPath -> IO ByteString
readBinaryFileIO = decodeUtf >=> BS.readFile

-- | @since 0.1
writeBinaryFileIO :: OsPath -> ByteString -> IO ()
writeBinaryFileIO p bs = decodeUtf p >>= \p' -> BS.writeFile p' bs

-- | @since 0.1
appendBinaryFileIO :: OsPath -> ByteString -> IO ()
appendBinaryFileIO p bs = decodeUtf p >>= \p' -> BS.appendFile p' bs

-- | @since 0.1
openBinaryFileIO :: OsPath -> IOMode -> IO Handle
openBinaryFileIO p m = decodeUtf p >>= \h -> IO.openBinaryFile h m

-- | @since 0.1
withBinaryFileIO :: OsPath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFileIO p m f = decodeUtf p >>= \h -> IO.withBinaryFile h m f
