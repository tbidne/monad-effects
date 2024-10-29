module PosixCompat (tests) where

import Control.Exception.Utils (trySync)
import Data.ByteString qualified as BS
import Effects.System.PosixCompat
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
  )
import Effects.System.PosixCompat qualified as PC
import System.Directory qualified as Dir
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO FilePath -> TestTree
tests getTmpDir =
  testGroup
    "PosixCompat"
    [ pathTypeTests getTmpDir
    ]

pathTypeTests :: IO FilePath -> TestTree
pathTypeTests getTestDir =
  testGroup
    "PathType"
    [ getPathTypeSymlink getTestDir,
      getPathTypeDirectory getTestDir,
      getPathTypeFile getTestDir,
      getPathTypeBad getTestDir
    ]

getPathTypeSymlink :: IO FilePath -> TestTree
getPathTypeSymlink getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir "getPathTypeSymlink"

  let link1 = testDir </> "file-link"
      link2 = testDir </> "dir-link"

  -- getPathType
  pathType1 <- PC.getPathType link1
  PathTypeSymbolicLink @=? pathType1

  -- isPathType
  isSymlink <- PC.isPathType PathTypeSymbolicLink link1
  assertBool "Should be a symlink" isSymlink

  isDirectory <- PC.isPathType PathTypeDirectory link1
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- PC.isPathType PathTypeFile link1
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link1
  throwIfNoEx $ throwHelper PathTypeDirectory link1
  throwIfNoEx $ throwHelper PathTypeFile link1

  -- getPathType
  pathType2 <- PC.getPathType (testDir </> "dir-link")
  PathTypeSymbolicLink @=? pathType2

  -- isPathType
  isSymlink2 <- PC.isPathType PathTypeSymbolicLink link2
  assertBool "Should be a symlink" isSymlink2

  isDirectory2 <- PC.isPathType PathTypeDirectory link2
  assertBool "Should not be a directory" (not isDirectory2)

  isFile2 <- PC.isPathType PathTypeFile link2
  assertBool "Should not be a file" (not isFile2)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link2
  throwIfNoEx $ throwHelper PathTypeDirectory link2
  throwIfNoEx $ throwHelper PathTypeFile link2
  where
    desc = "getPathType recognizes symlinks"
    throwHelper = PC.throwIfWrongPathType "getPathTypeSymlink"

getPathTypeDirectory :: IO FilePath -> TestTree
getPathTypeDirectory getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir "getPathTypeDirectory"

  -- getPathType
  pathType <- PC.getPathType testDir
  PathTypeDirectory @=? pathType

  -- isPathType
  isSymlink <- PC.isPathType PathTypeSymbolicLink testDir
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- PC.isPathType PathTypeDirectory testDir
  assertBool "Should be a directory" isDirectory

  isFile <- PC.isPathType PathTypeFile testDir
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink testDir
  throwHelper PathTypeDirectory testDir
  throwIfNoEx $ throwHelper PathTypeFile testDir
  where
    desc = "getPathType recognizes directories"
    throwHelper = PC.throwIfWrongPathType "getPathTypeDirectory"

getPathTypeFile :: IO FilePath -> TestTree
getPathTypeFile getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir "getPathTypeFile"
  let path = testDir </> "file"

  -- getPathType
  pathType <- PC.getPathType path
  PathTypeFile @=? pathType

  -- isPathType
  isSymlink <- PC.isPathType PathTypeSymbolicLink path
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- PC.isPathType PathTypeDirectory path
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- PC.isPathType PathTypeFile path
  assertBool "Should be a file" isFile

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink path
  throwIfNoEx $ throwHelper PathTypeDirectory path
  PC.throwIfWrongPathType "" PathTypeFile path
  where
    desc = "getPathType recognizes files"
    throwHelper = PC.throwIfWrongPathType "getPathTypeFile"

getPathTypeBad :: IO FilePath -> TestTree
getPathTypeBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir "getPathTypeBad"

  eResult <- trySync $ PC.getPathType (testDir </> "bad file")

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

setupLinks :: IO FilePath -> FilePath -> IO FilePath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> "posix-compat" </> suffix) <$> getTestDir
  let fileLink = testDir </> "file-link"
      dirLink = testDir </> "dir-link"
      file = testDir </> "file"
      dir = testDir </> "dir"

  Dir.createDirectoryIfMissing True dir
  BS.writeFile file ""
  Dir.createFileLink file fileLink
  Dir.createDirectoryLink dir dirLink

  pure testDir

throwIfNoEx :: IO a -> IO ()
throwIfNoEx m = do
  trySync m >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
