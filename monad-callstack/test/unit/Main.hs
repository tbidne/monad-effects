module Main (main) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Effects.MonadCallStack
  ( MonadCallStack (addCallStack, throwWithCallStack),
    displayCallStack,
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
        addsCallStack
      ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsCallStack :: TestTree
throwsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @SomeException (throwWithCallStack MkEx) <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Throws with callstack"
    gpath = goldenPath </> "throw-callstack.golden"

addsCallStack :: TestTree
addsCallStack =
  goldenVsStringDiff desc diff gpath $
    try @SomeException (addCallStack $ throwIO MkEx) <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Adds callstack"
    gpath = goldenPath </> "add-callstack.golden"

goldenPath :: FilePath
goldenPath = "test/unit/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

stableCallStack :: Exception e => e -> String
stableCallStack = unlines . take 2 . lines . displayCallStack
