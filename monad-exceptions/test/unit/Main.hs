module Main (main) where

import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Effects.Exception
  ( Exception,
    SomeException,
    addCallStack,
    displayCallStack,
    displayNoCallStack,
    throwM,
    throwWithCallStack,
    try,
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ throwsCallStack,
        addsCallStack,
        displaysNoCallStack
      ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsCallStack :: TestTree
throwsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @_ @SomeException (throwWithCallStack MkEx) <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Throws with callstack"
    gpath = goldenPath </> "throw-callstack.golden"

addsCallStack :: TestTree
addsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @_ @SomeException (addCallStack $ throwM MkEx) <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Adds callstack"
    gpath = goldenPath </> "add-callstack.golden"

displaysNoCallStack :: TestTree
displaysNoCallStack =
  goldenVsStringDiff desc diff gpath $
    try @_ @SomeException (throwWithCallStack MkEx) <&> \case
      Left e -> fromString $ displayNoCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Does not display callstack"
    gpath = goldenPath </> "no-callstack.golden"

goldenPath :: FilePath
goldenPath = "test/unit/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

stableCallStack :: Exception e => e -> String
stableCallStack = unlines . take 2 . lines . displayCallStack