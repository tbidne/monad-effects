{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Posix (tests) where

import Data.ByteString qualified as BS
import Effects.Exception (tryAny)
import Effects.FileSystem.OsPath (OsPath, osp, (</>))
import Effects.FileSystem.OsPath qualified as FS.OsPath
import Effects.System.Posix
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
  )
import Effects.System.Posix qualified as P
import System.Directory.OsPath qualified as Dir
import System.OsString.Internal.Types (OsString (getOsString))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PosixCompat"
    [ pathTypeTests getTmpDir
    ]

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

  let link1 = (testDir </> [osp|file-link|]).getOsString
      link2 = (testDir </> [osp|dir-link|]).getOsString

  -- getPathType
  pathType1 <- P.getPathType link1
  PathTypeSymbolicLink @=? pathType1

  -- isPathType
  isSymlink <- P.isPathType PathTypeSymbolicLink link1
  assertBool "Should be a symlink" isSymlink

  isDirectory <- P.isPathType PathTypeDirectory link1
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- P.isPathType PathTypeFile link1
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link1
  throwIfNoEx $ throwHelper PathTypeDirectory link1
  throwIfNoEx $ throwHelper PathTypeFile link1

  -- getPathType
  pathType2 <- P.getPathType (testDir </> [osp|dir-link|]).getOsString
  PathTypeSymbolicLink @=? pathType2

  -- isPathType
  isSymlink2 <- P.isPathType PathTypeSymbolicLink link2
  assertBool "Should be a symlink" isSymlink2

  isDirectory2 <- P.isPathType PathTypeDirectory link2
  assertBool "Should not be a directory" (not isDirectory2)

  isFile2 <- P.isPathType PathTypeFile link2
  assertBool "Should not be a file" (not isFile2)

  -- throwIfWrongPathType
  throwHelper PathTypeSymbolicLink link2
  throwIfNoEx $ throwHelper PathTypeDirectory link2
  throwIfNoEx $ throwHelper PathTypeFile link2
  where
    desc = "getPathType recognizes symlinks"
    throwHelper = P.throwIfWrongPathType "getPathTypeSymlink"

getPathTypeDirectory :: IO OsPath -> TestTree
getPathTypeDirectory getTestDir = testCase desc $ do
  testDir <- (.getOsString) <$> setupLinks getTestDir [osp|getPathTypeDirectory|]

  -- getPathType
  pathType <- P.getPathType testDir
  PathTypeDirectory @=? pathType

  -- isPathType
  isSymlink <- P.isPathType PathTypeSymbolicLink testDir
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- P.isPathType PathTypeDirectory testDir
  assertBool "Should be a directory" isDirectory

  isFile <- P.isPathType PathTypeFile testDir
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink testDir
  throwHelper PathTypeDirectory testDir
  throwIfNoEx $ throwHelper PathTypeFile testDir
  where
    desc = "getPathType recognizes directories"
    throwHelper = P.throwIfWrongPathType "getPathTypeDirectory"

getPathTypeFile :: IO OsPath -> TestTree
getPathTypeFile getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|getPathTypeFile|]
  let path = (testDir </> [osp|file|]).getOsString

  -- getPathType
  pathType <- P.getPathType path
  PathTypeFile @=? pathType

  -- isPathType
  isSymlink <- P.isPathType PathTypeSymbolicLink path
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- P.isPathType PathTypeDirectory path
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- P.isPathType PathTypeFile path
  assertBool "Should be a file" isFile

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink path
  throwIfNoEx $ throwHelper PathTypeDirectory path
  P.throwIfWrongPathType "" PathTypeFile path
  where
    desc = "getPathType recognizes files"
    throwHelper = P.throwIfWrongPathType "getPathTypeFile"

getPathTypeBad :: IO OsPath -> TestTree
getPathTypeBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|getPathTypeBad|]

  eResult <- tryAny $ P.getPathType (testDir </> [osp|bad file|]).getOsString

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|posix|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

      file' = FS.OsPath.decodeLenient file

  Dir.createDirectoryIfMissing True dir
  BS.writeFile file' ""
  Dir.createFileLink file fileLink
  Dir.createDirectoryLink dir dirLink

  pure testDir

throwIfNoEx :: IO a -> IO ()
throwIfNoEx m = do
  tryAny m >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
