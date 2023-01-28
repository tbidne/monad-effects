module PathReader (tests) where

import Data.String (IsString (fromString))
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader qualified as PathReader
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

tests :: TestTree
tests =
  testGroup
    "PathReader"
    [ testListDirectoryRecursive
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = goldenVsStringDiff desc diff gpath $ do
  (files, dirs) <- PathReader.listDirectoryRecursive "./src"
  let str =
        mconcat
          [ "FILES\n\n",
            unlines files,
            "\nDIRECTORIES\n\n",
            unlines dirs
          ]
  pure $ fromString str
  where
    desc = "Recursively lists sub-files/dirs"
    gpath = goldenPath </> "listDirectoryRecursive.golden"

goldenPath :: FilePath
goldenPath = "test/unit/PathReader"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]
