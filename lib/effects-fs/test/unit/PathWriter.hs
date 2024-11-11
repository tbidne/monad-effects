{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PathWriter (tests) where

import Effects.FileSystem.FileWriter
  ( OsPath,
  )
import Effects.FileSystem.PathWriter qualified as PW
import FileSystem.OsPath (osp, (</>))
import PathWriter.CopyDir qualified
import PathWriter.Symlink qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestUtils qualified

tests :: IO OsPath -> TestTree
tests getTmpDir = do
  testGroup
    "PathWriter"
    [ PathWriter.CopyDir.tests getTmpDir,
      removeExistsTests getTmpDir,
      PathWriter.Symlink.tests getTmpDir
    ]

-- NOTE: For removeExistsTests, we do not test all permutations. In particular,
-- we do not test symlinks as "bad paths" for e.g. removeFileIfExists or
-- removeDirIfExists because those functions are based on
-- does(file|directory)Exist, and those return True based on the _target_
-- for the link.
--
-- In other words, for an extant directory link, doesDirectoryExist will return
-- true, yet removeDirectory will throw an exception.
--
-- doesFileExist / removeFile will work on Posix because Posix treats symlinks
-- as files...but it wil fail on windows.
--
-- But we want to keep these functions as simple as possible i.e. the obvious
-- doesXExist -> removeX. So we don't maintain any illusion that these
-- functions are total for all possible path type inputs. Really you should
-- only use them when you know the type of your potential path.

removeExistsTests :: IO OsPath -> TestTree
removeExistsTests getTestDir =
  testGroup
    "removeXIfExists"
    [ removeFileIfExistsTrue getTestDir,
      removeFileIfExistsFalseBad getTestDir,
      removeFileIfExistsFalseWrongType getTestDir,
      removeDirIfExistsTrue getTestDir,
      removeDirIfExistsFalseBad getTestDir,
      removeDirIfExistsFalseWrongType getTestDir,
      removeSymlinkIfExistsTrue getTestDir,
      removeSymlinkIfExistsFalseBad getTestDir,
      removeSymlinkIfExistsFalseWrongType getTestDir
    ]

removeFileIfExistsTrue :: IO OsPath -> TestTree
removeFileIfExistsTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeFileIfExistsTrue|]
  let file = testDir </> [osp|file|]
  TestUtils.assertFilesExist [file]

  deleted <- PW.removeFileIfExists file
  assertBool "Should run delete" deleted

  TestUtils.assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists removes file"

removeFileIfExistsFalseBad :: IO OsPath -> TestTree
removeFileIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeFileIfExistsFalseBad|]
  let file = testDir </> [osp|bad-path|]
  TestUtils.assertFilesDoNotExist [file]

  deleted <- PW.removeFileIfExists file
  assertBool "Should not run delete" (not deleted)

  TestUtils.assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists does nothing for bad path"

removeFileIfExistsFalseWrongType :: IO OsPath -> TestTree
removeFileIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeFileIfExistsFalseWrongType|]
  let dir = testDir </> [osp|dir|]

  TestUtils.assertDirsExist [dir]

  deleted <- PW.removeFileIfExists dir
  assertBool "Should not run delete" (not deleted)

  TestUtils.assertDirsExist [dir]
  where
    desc = "removeFileIfExists does nothing for wrong file types"

removeDirIfExistsTrue :: IO OsPath -> TestTree
removeDirIfExistsTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeDirIfExistsTrue|]
  let dir = testDir </> [osp|dir|]
  TestUtils.assertDirsExist [dir]

  deleted <- PW.removeDirectoryIfExists dir
  assertBool "Should run delete" deleted

  TestUtils.assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists removes dir"

removeDirIfExistsFalseBad :: IO OsPath -> TestTree
removeDirIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeDirIfExistsFalseBad|]
  let dir = testDir </> [osp|bad-path|]
  TestUtils.assertDirsDoNotExist [dir]

  deleted <- PW.removeDirectoryIfExists dir
  assertBool "Should not run delete" (not deleted)

  TestUtils.assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists does nothing for bad path"

removeDirIfExistsFalseWrongType :: IO OsPath -> TestTree
removeDirIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeDirIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]

  TestUtils.assertFilesExist [file]

  deleted <- PW.removeDirectoryIfExists file
  assertBool "Should not run delete" (not deleted)

  TestUtils.assertFilesExist [file]
  where
    desc = "removeDirectoryIfExists does nothing for wrong file types"

removeSymlinkIfExistsTrue :: IO OsPath -> TestTree
removeSymlinkIfExistsTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymlinkIfExistsTrue|]
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]

  TestUtils.assertSymlinksExist [fileLink, dirLink]

  deleted1 <- PW.removeSymbolicLinkIfExists fileLink
  assertBool "Should run delete" deleted1

  deleted2 <- PW.removeSymbolicLinkIfExists dirLink
  assertBool "Should run delete" deleted2

  TestUtils.assertSymlinksDoNotExist [fileLink, dirLink]
  where
    desc = "removeSymbolicLinkIfExists removes links"

removeSymlinkIfExistsFalseBad :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymlinkIfExistsFalseBad|]
  let link = testDir </> [osp|bad-path|]
  TestUtils.assertSymlinksDoNotExist [link]

  deleted <- PW.removeSymbolicLinkIfExists link
  assertBool "Should not run delete" (not deleted)

  TestUtils.assertSymlinksDoNotExist [link]
  where
    desc = "removeSymbolicLinkIfExists does nothing for bad path"

removeSymlinkIfExistsFalseWrongType :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymlinkIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  TestUtils.assertFilesExist [file]
  TestUtils.assertDirsExist [dir]

  deleted1 <- PW.removeSymbolicLinkIfExists file
  assertBool "Should not run delete" (not deleted1)

  deleted2 <- PW.removeSymbolicLinkIfExists dir
  assertBool "Should not run delete" (not deleted2)

  TestUtils.assertFilesExist [file]
  TestUtils.assertDirsExist [dir]
  where
    desc = "removeSymbolicLinkIfExists does nothing for wrong file types"
