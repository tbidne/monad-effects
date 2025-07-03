module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Effects.Logger qualified as Effects.Logger
import Unit.Effects.Logger.Namespace qualified as Effects.Logger.Namespace

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effects.Logger"
      [ Effects.Logger.tests,
        Effects.Logger.Namespace.tests
      ]
