{-# LANGUAGE QuasiQuotes #-}

module PathReader (tests) where

import Control.Monad (zipWithM_)
import Data.List qualified as L
import Effects.FileSystem.PathReader qualified as PathReader
import Effects.FileSystem.Utils (osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "PathReader"
    [ testListDirectoryRecursive
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <- PathReader.listDirectoryRecursive [osp|src|]
  let (files', dirs') = (L.sort files, L.sort dirs)
  zipWithM_ (@=?) expectedFiles files'
  zipWithM_ (@=?) expectedDirs dirs'
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
