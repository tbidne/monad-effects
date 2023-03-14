module PathReader (tests) where

import Control.Monad (zipWithM_)
import Data.List qualified as L
import Effects.FileSystem.PathReader qualified as PathReader
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
  (files, dirs) <- PathReader.listDirectoryRecursive "./src"
  let (files', dirs') = (L.sort files, L.sort dirs)
  zipWithM_ (@=?) expectedFiles files'
  zipWithM_ (@=?) expectedDirs dirs'
  where
    expectedFiles =
      [ "Effects/FileSystem/FileReader.hs",
        "Effects/FileSystem/FileWriter.hs",
        "Effects/FileSystem/HandleReader.hs",
        "Effects/FileSystem/HandleWriter.hs",
        "Effects/FileSystem/Internal.hs",
        "Effects/FileSystem/Path.hs",
        "Effects/FileSystem/PathReader.hs",
        "Effects/FileSystem/PathWriter.hs"
      ]
    expectedDirs =
      [ "Effects",
        "Effects/FileSystem"
      ]
