module Main (main) where

import Annotation.CallStack.Legacy qualified as CallStack.Legacy
import Annotation.Utils qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effects.Exceptions"
      [ Annotation.Utils.tests,
        CallStack.Legacy.tests
      ]
