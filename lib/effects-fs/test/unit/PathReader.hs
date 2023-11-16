{-# LANGUAGE QuasiQuotes #-}

module PathReader (tests) where

import Data.List qualified as L
import Effects.FileSystem.PathReader qualified as PathReader
import Effects.FileSystem.Utils (osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "PathReader"
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
