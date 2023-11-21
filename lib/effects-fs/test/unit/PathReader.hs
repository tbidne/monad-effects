{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module PathReader (tests) where

import Data.List qualified as L
import Effects.Exception (tryAny)
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathReader (PathType (PathTypeDirectory, PathTypeFile, PathTypeSymbolicLink))
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathReader qualified as PathReader
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.Utils (OsPath, osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PathReader"
    [ listDirectoryTests,
      symlinkTests getTmpDir,
      pathTypeTests getTmpDir
    ]

listDirectoryTests :: TestTree
listDirectoryTests =
  testGroup
    "listDirectoryRecursive"
    [ testListDirectoryRecursive,
      testListDirectoryRecursiveSymlinkTargets,
      testListDirectoryRecursiveSymbolicLink
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <- PathReader.listDirectoryRecursive [osp|src|]
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
        prefix </> [osp|PathWriter.hs|],
        prefix </> [osp|Utils.hs|]
      ]
    expectedDirs =
      [ [osp|Effects|],
        prefix
      ]

    prefix = [osp|Effects|] </> [osp|FileSystem|]

testListDirectoryRecursiveSymlinkTargets :: TestTree
testListDirectoryRecursiveSymlinkTargets = testCase desc $ do
  (files, dirs) <- PathReader.listDirectoryRecursive dataDir
  let (files', dirs') = (L.sort files, L.sort dirs)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    desc = "Symlinks are categorized via targets"
    dataDir = [osp|test|] </> [osp|data|]
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

testListDirectoryRecursiveSymbolicLink :: TestTree
testListDirectoryRecursiveSymbolicLink = testCase desc $ do
  (files, dirs, symlinks) <- PathReader.listDirectoryRecursiveSymbolicLink dataDir
  let (files', dirs', symlinks') = (L.sort files, L.sort dirs, L.sort symlinks)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  expectedSymlinks @=? symlinks'
  where
    desc = "Recursively lists sub-files/dirs/symlinks"
    dataDir = [osp|test|] </> [osp|data|]
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
  testDir <- setupLinks getTestDir [osp|doesSymbolicLinkExistTrue|]

  fileLinkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|file-link|])
  assertBool "doesSymbolicLinkExist true for file link" fileLinkExists

  dirLinkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|dir-link|])
  assertBool "doesSymbolicLinkExist true for dir link" dirLinkExists
  where
    desc = "doesSymbolicLinkExist true for symlinks"

doesSymbolicLinkExistFileFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistFileFalse getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|doesSymbolicLinkExistFileFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|file|])
  assertBool "doesSymbolicLinkExist false for file" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for file"

doesSymbolicLinkExistDirFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistDirFalse getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|doesSymbolicLinkExistDirFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|dir|])
  assertBool "doesSymbolicLinkExist false for dir" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for dir"

doesSymbolicLinkExistBadFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistBadFalse getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|doesSymbolicLinkExistBadFalse|]

  linkExists <- PR.doesSymbolicLinkExist (testDir </> [osp|bad-path|])
  assertBool "doesSymbolicLinkExist false for bad path" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for bad path"

{- ORMOLU_DISABLE -}

pathIsSymbolicDirectoryLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicDirectoryLinkTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkTrue|]

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir-link|])
  assertBool "pathIsSymbolicDirectoryLink true for dir link" isDirLink
  where
    desc = "pathIsSymbolicDirectoryLink true for dir link"

pathIsSymbolicFileLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicFileLinkTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicFileLinkTrue|]

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
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkFalse|]

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file-link|])
  assertBool "pathIsSymbolicDirectoryLink false for windows file link" (not isDirLink)
  where
    desc = "pathIsSymbolicDirectoryLink false for windows file link"

pathIsSymbolicFileLinkFalse :: IO OsPath -> TestTree
pathIsSymbolicFileLinkFalse getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicFileLinkFalse|]

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
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicFileLinkFileFalse|]

  isFileLink <- PR.pathIsSymbolicFileLink (testDir </> [osp|file|])
  assertBool "pathIsSymbolicFileLink false for file" (not isFileLink)

  isFileLink2 <- PR.pathIsSymbolicFileLink (testDir </> [osp|dir|])
  assertBool "pathIsSymbolicFileLink false for dir" (not isFileLink2)

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file|])
  assertBool "pathIsSymbolicDirectoryLink false for file" (not isDirLink)

  isDirLink2 <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir|])
  assertBool "pathIsSymbolicDirectoryLink false for dir" (not isDirLink2)
  where
    desc = "pathIsSymbolicXLink false for non symlinks"

pathIsSymbolicFileLinkBad :: IO OsPath -> TestTree
pathIsSymbolicFileLinkBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|pathIsSymbolicFileLinkBad|]

  isFileLink <- PR.pathIsSymbolicFileLink (testDir </> [osp|bad|])
  assertBool "pathIsSymbolicFileLink false for bad path" (not isFileLink)

  isDirLink <- PR.pathIsSymbolicDirectoryLink (testDir </> [osp|bad|])
  assertBool "pathIsSymbolicFileLink false for bad path" (not isDirLink)
  where
    desc = "pathIsSymbolicXLink false for bad path"

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
  testDir <- setupLinks getTestDir [osp|getPathTypeSymlink|]

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
  testDir <- setupLinks getTestDir [osp|getPathTypeDirectory|]

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
  testDir <- setupLinks getTestDir [osp|getPathTypeFile|]
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
  testDir <- setupLinks getTestDir [osp|getPathTypeBad|]

  eResult <- tryAny $ PR.getPathType (testDir </> [osp|bad file|])

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|path-reader|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  PW.createDirectoryIfMissing True dir
  FW.writeBinaryFile file ""
  PW.createFileLink file fileLink
  PW.createDirectoryLink dir dirLink

  pure testDir

throwIfNoEx :: IO a -> IO ()
throwIfNoEx m = do
  tryAny m >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
