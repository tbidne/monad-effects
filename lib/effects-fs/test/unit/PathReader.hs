{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module PathReader (tests) where

import Control.Exception.Utils (trySync)
import Data.List qualified as L
import Effects.FileSystem.PathReader
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
  )
import Effects.FileSystem.PathReader qualified as PR
import FileSystem.OsPath (OsPath, osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import TestUtils qualified

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PathReader"
    [ listDirectoryTests getTmpDir,
      symlinkTests getTmpDir,
      pathTypeTests getTmpDir
    ]

listDirectoryTests :: IO OsPath -> TestTree
listDirectoryTests getTmpDir =
  testGroup
    "listDirectoryRecursive"
    [ testListDirectoryRecursive,
      testListDirectoryRecursiveSymlinkTargets getTmpDir,
      testListDirectoryRecursiveSymbolicLink getTmpDir
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <- PR.listDirectoryRecursive [osp|src|]
  let (files', dirs') = (L.sort files, L.sort dirs)
  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    expectedFiles =
      [ prefix </> [osp|FileReader.hs|],
        prefix </> [osp|FileWriter.hs|],
        prefix </> [osp|HandleReader.hs|],
        prefix </> [osp|HandleWriter.hs|],
        prefix </> [osp|PathReader.hs|],
        prefix </> [osp|PathWriter.hs|]
      ]
    expectedDirs =
      [ [osp|Effects|],
        prefix
      ]

    prefix = [osp|Effects|] </> [osp|FileSystem|]

testListDirectoryRecursiveSymlinkTargets :: IO OsPath -> TestTree
testListDirectoryRecursiveSymlinkTargets getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs) <- PR.listDirectoryRecursive dataDir
  let (files', dirs') = (L.sort files, L.sort dirs)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    desc = "Symlinks are categorized via targets"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|],
        [osp|l1|],
        [osp|l2|] </> [osp|f|],
        [osp|l3|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|],
        [osp|l2|]
      ]

testListDirectoryRecursiveSymbolicLink :: IO OsPath -> TestTree
testListDirectoryRecursiveSymbolicLink getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs, symlinks) <- PR.listDirectoryRecursiveSymbolicLink dataDir

  let (files', dirs', symlinks') = (L.sort files, L.sort dirs, L.sort symlinks)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  expectedSymlinks @=? symlinks'
  where
    desc = "Recursively lists sub-files/dirs/symlinks"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|]
      ]
    expectedSymlinks =
      [ [osp|l1|],
        [osp|l2|],
        [osp|l3|]
      ]

symlinkTests :: IO OsPath -> TestTree
symlinkTests getTestDir =
  testGroup
    "Symlinks"
    ( [ doesSymbolicLinkExistTrue getTestDir,
        doesSymbolicLinkExistFileFalse getTestDir,
        doesSymbolicLinkExistDirFalse getTestDir,
        doesSymbolicLinkExistBadFalse getTestDir,
        pathIsSymbolicDirectoryLinkTrue getTestDir,
        pathIsSymbolicFileLinkTrue getTestDir,
        pathIsSymbolicFileLinkFileFalse getTestDir,
        pathIsSymbolicFileLinkBad getTestDir
      ]
        ++ windowsTests getTestDir
    )

doesSymbolicLinkExistTrue :: IO OsPath -> TestTree
doesSymbolicLinkExistTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistTrue|]

  fileLinkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|file-link|])
  assertBool "doesSymbolicLinkExist true for file link" fileLinkExists

  dirLinkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|dir-link|])
  assertBool "doesSymbolicLinkExist true for dir link" dirLinkExists
  where
    desc = "doesSymbolicLinkExist true for symlinks"

doesSymbolicLinkExistFileFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistFileFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistFileFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|file|])
  assertBool "doesSymbolicLinkExist false for file" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for file"

doesSymbolicLinkExistDirFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistDirFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistDirFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|dir|])
  assertBool "doesSymbolicLinkExist false for dir" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for dir"

doesSymbolicLinkExistBadFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistBadFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistBadFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|bad-path|])
  assertBool "doesSymbolicLinkExist false for bad path" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for bad path"

{- ORMOLU_DISABLE -}

pathIsSymbolicDirectoryLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicDirectoryLinkTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkTrue|]

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir-link|])
  assertBool "pathIsSymbolicDirectoryLink true for dir link" isDirLink
  where
    desc = "pathIsSymbolicDirectoryLink true for dir link"

pathIsSymbolicFileLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicFileLinkTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkTrue|]

  isFileLink <- PR.pathIsSymbolicFileLink (testDir </> [osp|file-link|])
  assertBool "pathIsSymbolicFileLink true for file link" isFileLink
  where
    desc = "pathIsSymbolicDirectoryLink true for file link"

windowsTests :: IO OsPath -> [TestTree]
#if WINDOWS
windowsTests getTestDir =
  [ pathIsSymbolicDirectoryLinkFalse getTestDir,
    pathIsSymbolicFileLinkFalse getTestDir
  ]

pathIsSymbolicDirectoryLinkFalse :: IO OsPath -> TestTree
pathIsSymbolicDirectoryLinkFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkFalse|]

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file-link|])
  assertBool "pathIsSymbolicDirectoryLink false for windows file link" (not isDirLink)
  where
    desc = "pathIsSymbolicDirectoryLink false for windows file link"

pathIsSymbolicFileLinkFalse :: IO OsPath -> TestTree
pathIsSymbolicFileLinkFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkFalse|]

  isFileLink <- PR.pathIsSymbolicFileLink (testDir </> [osp|dir-link|])
  assertBool "pathIsSymbolicFileLink false for windows dir link" (not isFileLink)
  where
    desc = "pathIsSymbolicFileLink false for windows dir link"
#else
windowsTests _ = [ ]
#endif

{- ORMOLU_ENABLE -}

pathIsSymbolicFileLinkFileFalse :: IO OsPath -> TestTree
pathIsSymbolicFileLinkFileFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkFileFalse|]

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|file|])

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|dir|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir|])
  where
    desc = "pathIsSymbolicXLink exception for non symlinks"

pathIsSymbolicFileLinkBad :: IO OsPath -> TestTree
pathIsSymbolicFileLinkBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkBad|]

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|bad|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|bad|])
  where
    desc = "pathIsSymbolicXLink exception for bad path"

pathTypeTests :: IO OsPath -> TestTree
pathTypeTests getTestDir =
  testGroup
    "PathType"
    [ getPathTypeSymlink getTestDir,
      getPathTypeDirectory getTestDir,
      getPathTypeFile getTestDir,
      getPathTypeBad getTestDir
    ]

getPathTypeSymlink :: IO OsPath -> TestTree
getPathTypeSymlink getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeSymlink|]

  let link1 = testDir </> [osp|file-link|]
      link2 = testDir </> [osp|dir-link|]

  -- getPathType
  pathType1 <- PR.getPathType link1
  PathTypeSymbolicLink @=? pathType1

  -- isPathType
  isSymlink <- PR.isPathType PathTypeSymbolicLink link1
  assertBool "Should be a symlink" isSymlink

  isDirectory <- PR.isPathType PathTypeDirectory link1
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- PR.isPathType PathTypeFile link1
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link1
  throwIfNoEx $ throwHelper PathTypeDirectory link1
  throwIfNoEx $ throwHelper PathTypeFile link1

  -- getPathType
  pathType2 <- PR.getPathType (testDir </> [osp|dir-link|])
  PathTypeSymbolicLink @=? pathType2

  -- isPathType
  isSymlink2 <- PR.isPathType PathTypeSymbolicLink link2
  assertBool "Should be a symlink" isSymlink2

  isDirectory2 <- PR.isPathType PathTypeDirectory link2
  assertBool "Should not be a directory" (not isDirectory2)

  isFile2 <- PR.isPathType PathTypeFile link2
  assertBool "Should not be a file" (not isFile2)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link2
  throwIfNoEx $ throwHelper PathTypeDirectory link2
  throwIfNoEx $ throwHelper PathTypeFile link2
  where
    desc = "getPathType recognizes symlinks"
    throwHelper = PR.throwIfWrongPathType "getPathTypeSymlink"

getPathTypeDirectory :: IO OsPath -> TestTree
getPathTypeDirectory getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeDirectory|]

  -- getPathType
  pathType <- PR.getPathType testDir
  PathTypeDirectory @=? pathType

  -- isPathType
  isSymlink <- PR.isPathType PathTypeSymbolicLink testDir
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- PR.isPathType PathTypeDirectory testDir
  assertBool "Should be a directory" isDirectory

  isFile <- PR.isPathType PathTypeFile testDir
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink testDir
  throwHelper PathTypeDirectory testDir
  throwIfNoEx $ throwHelper PathTypeFile testDir
  where
    desc = "getPathType recognizes directories"
    throwHelper = PR.throwIfWrongPathType "getPathTypeDirectory"

getPathTypeFile :: IO OsPath -> TestTree
getPathTypeFile getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeFile|]
  let path = testDir </> [osp|file|]

  -- getPathType
  pathType <- PR.getPathType path
  PathTypeFile @=? pathType

  -- isPathType
  isSymlink <- PR.isPathType PathTypeSymbolicLink path
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- PR.isPathType PathTypeDirectory path
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- PR.isPathType PathTypeFile path
  assertBool "Should be a file" isFile

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink path
  throwIfNoEx $ throwHelper PathTypeDirectory path
  PR.throwIfWrongPathType "" PathTypeFile path
  where
    desc = "getPathType recognizes files"
    throwHelper = PR.throwIfWrongPathType "getPathTypeFile"

getPathTypeBad :: IO OsPath -> TestTree
getPathTypeBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeBad|]

  eResult <- trySync $ PR.getPathType (testDir </> [osp|bad file|])

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

throwIfNoEx :: IO a -> IO ()
throwIfNoEx m = do
  trySync m >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
