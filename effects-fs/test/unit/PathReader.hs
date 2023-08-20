module PathReader (tests) where

import Control.Monad (zipWithM_)
import Data.List qualified as L
import Effects.FileSystem.PathReader qualified as PathReader
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Utils qualified as U

tests :: TestTree
tests =
  testGroup
    "PathReader"
    [ testListDirectoryRecursive
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <- PathReader.listDirectoryRecursive (U.strToPath "./src")
  let (files', dirs') = (L.sort files, L.sort dirs)
  zipWithM_ (@=?) expectedFiles (U.pathToStr <$> files')
  zipWithM_ (@=?) expectedDirs (U.pathToStr <$> dirs')
  where
    expectedFiles =
      [ "Effects" </> "FileSystem" </> "FileReader.hs",
        "Effects" </> "FileSystem" </> "FileWriter.hs",
        "Effects" </> "FileSystem" </> "HandleReader.hs",
        "Effects" </> "FileSystem" </> "HandleWriter.hs",
        "Effects" </> "FileSystem" </> "PathReader.hs",
        "Effects" </> "FileSystem" </> "PathWriter.hs",
        "Effects" </> "FileSystem" </> "Utils.hs"
      ]
    expectedDirs =
      [ "Effects",
        "Effects" </> "FileSystem"
      ]
