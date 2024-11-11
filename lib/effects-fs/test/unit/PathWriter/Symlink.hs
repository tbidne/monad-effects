{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PathWriter.Symlink (tests) where

import Control.Exception (IOException)
import Control.Monad.Catch (try)
import Effects.FileSystem.FileWriter
  ( OsPath,
  )
import Effects.FileSystem.PathWriter qualified as PW
import FileSystem.OsPath (osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestUtils qualified

tests :: IO OsPath -> TestTree
tests getTmpDir = do
  testGroup
    "Symlinks"
    [ removeLinkTests getTmpDir,
      copyLinkTests getTmpDir
    ]

removeLinkTests :: IO OsPath -> TestTree
removeLinkTests getTestDir =
  testGroup
    "removeSymbolicLink"
    [ removeSymbolicLinkFileLink getTestDir,
      removeSymbolicLinkFileException getTestDir,
      removeSymbolicLinkBadException getTestDir
    ]

removeSymbolicLinkFileLink :: IO OsPath -> TestTree
removeSymbolicLinkFileLink getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]

  TestUtils.assertSymlinksExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]

  PW.removeSymbolicLink (testDir </> [osp|file-link|])
  PW.removeSymbolicLink (testDir </> [osp|dir-link|])

  TestUtils.assertSymlinksDoNotExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]
  where
    desc = "Removes symbolic links"

removeSymbolicLinkFileException :: IO OsPath -> TestTree
removeSymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]
  let filePath = testDir </> [osp|file|]

  TestUtils.assertFilesExist [filePath]

  try @_ @IOException (PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"

  TestUtils.assertFilesExist [filePath]
  where
    desc = "Exception for file"

removeSymbolicLinkBadException :: IO OsPath -> TestTree
removeSymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|removeSymbolicLinkBadException|]
  let filePath = testDir </> [osp|bad-path|]

  try @_ @IOException (PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"
  where
    desc = "Exception for bad path"

copyLinkTests :: IO OsPath -> TestTree
copyLinkTests getTestDir =
  testGroup
    "copySymbolicLink"
    [ copySymbolicLinks getTestDir,
      copySymbolicLinkFileException getTestDir,
      copySymbolicLinkDirException getTestDir,
      copySymbolicLinkBadException getTestDir
    ]

copySymbolicLinks :: IO OsPath -> TestTree
copySymbolicLinks getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|copyFileLink|]
  let srcFile = testDir </> [osp|file-link|]
      srcDir = testDir </> [osp|dir-link|]
      destFile = testDir </> [osp|file-link2|]
      destDir = testDir </> [osp|dir-link2|]

  TestUtils.assertSymlinksExist [srcFile, srcDir]
  TestUtils.assertSymlinksDoNotExist [destFile, destDir]

  PW.copySymbolicLink srcFile destFile
  PW.copySymbolicLink srcDir destDir

  TestUtils.assertSymlinksExistTarget
    [ (srcFile, testDir </> [osp|file|]),
      (destFile, testDir </> [osp|file|]),
      (srcDir, testDir </> [osp|dir|]),
      (destDir, testDir </> [osp|dir|])
    ]
  where
    desc = "Copies symbolic links"

copySymbolicLinkFileException :: IO OsPath -> TestTree
copySymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|copySymbolicLinkFileException|]
  let src = testDir </> [osp|file|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"

copySymbolicLinkDirException :: IO OsPath -> TestTree
copySymbolicLinkDirException getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|copySymbolicLinkDirException|]
  let src = testDir </> [osp|dir|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for directory"

copySymbolicLinkBadException :: IO OsPath -> TestTree
copySymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|copySymbolicLinkBadException|]
  let src = testDir </> [osp|bad-path|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"
