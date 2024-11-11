{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module TestUtils
  ( -- * Setup
    mkTestPath,
    writeFiles,

    -- ** Symbolic Links
    setupLinks,

    -- * Assertions
    assertFilesExist,
    assertFilesDoNotExist,
    assertSymlinksExist,
    assertSymlinksExistTarget,
    assertSymlinksDoNotExist,
    assertFileContents,
    assertDirsExist,
    assertDirsDoNotExist,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Effects.FileSystem.FileWriter
  ( MonadFileWriter (writeBinaryFile),
    OsPath,
  )
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathReader
  ( doesDirectoryExist,
    doesFileExist,
    doesSymbolicLinkExist,
    getSymbolicLinkTarget,
  )
import Effects.FileSystem.PathWriter qualified as PW
import FileSystem.OsPath (osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (assertBool, (@=?))

-------------------------------------------------------------------------------
--                                  Setup                                    --
-------------------------------------------------------------------------------

-- | @mkTestPath getPath s@ returns s appended to the path.
mkTestPath :: IO OsPath -> OsPath -> IO OsPath
mkTestPath getPath s = (</> s) <$> getPath

-- | Writes the files.
writeFiles :: (HasCallStack) => [(OsPath, ByteString)] -> IO ()
writeFiles = traverse_ (uncurry writeBinaryFile)

-- | Sets up symbolink links at the given path. E.g. for @setupLinks p s@,
-- we have:
--
-- @
-- λ. tree (p/path-writer/s) -a
-- .
-- ├── dir
-- ├── dir-link -> dir
-- ├── file
-- ├── file-link -> file
-- @
setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|path-writer|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  PW.createDirectoryIfMissing True dir
  FW.writeBinaryFile file ""
  PW.createFileLink file fileLink
  PW.createDirectoryLink dir dirLink

  pure testDir

-------------------------------------------------------------------------------
--                                Assertions                                 --
-------------------------------------------------------------------------------

-- | Assert the files exist.
assertFilesExist :: (HasCallStack) => [OsPath] -> IO ()
assertFilesExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> FS.OsPath.decodeLenient p) exists

-- | Assert the files do not exist.
assertFilesDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertFilesDoNotExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file not to exist: " <> FS.OsPath.decodeLenient p) (not exists)

-- | Assert the symbolic links exist.
assertSymlinksExist :: (HasCallStack) => [OsPath] -> IO ()
assertSymlinksExist = assertSymlinksExist' . fmap (,Nothing)

-- | Assert the symbolic links exist and points to the expected target.
assertSymlinksExistTarget :: (HasCallStack) => [(OsPath, OsPath)] -> IO ()
assertSymlinksExistTarget = assertSymlinksExist' . (fmap . fmap) Just

assertSymlinksExist' :: (HasCallStack) => [(OsPath, Maybe OsPath)] -> IO ()
assertSymlinksExist' = traverse_ $ \(l, t) -> do
  exists <- doesSymbolicLinkExist l
  assertBool ("Expected symlink to exist: " <> FS.OsPath.decodeLenient l) exists

  case t of
    Nothing -> pure ()
    Just expectedTarget -> do
      target <- getSymbolicLinkTarget l
      expectedTarget @=? target

-- | Assert the symbolic links do not exist.
assertSymlinksDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertSymlinksDoNotExist = traverse_ $ \l -> do
  exists <- doesSymbolicLinkExist l
  assertBool ("Expected symlink not to exist: " <> FS.OsPath.decodeLenient l) (not exists)

-- | Assert the files exist with the expected contents.
assertFileContents :: (HasCallStack) => [(OsPath, ByteString)] -> IO ()
assertFileContents = traverse_ $ \(p, expected) -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> FS.OsPath.decodeLenient p) exists
  actual <- readBinaryFile p
  expected @=? actual

-- | Assert the directories exist.
assertDirsExist :: (HasCallStack) => [OsPath] -> IO ()
assertDirsExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory to exist: " <> FS.OsPath.decodeLenient p) exists

-- | Assert the directories od not exist.
assertDirsDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertDirsDoNotExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory not to exist: " <> FS.OsPath.decodeLenient p) (not exists)
