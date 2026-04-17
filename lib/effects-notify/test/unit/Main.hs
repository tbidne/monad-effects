{-# LANGUAGE CPP #-}

module Main (main) where

import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup)
import Unit.Notify qualified
import Unit.NotifySystem qualified

main :: IO ()
main = guardOrElse' "TEST_UNIT" ExpectEnvSet runTests dontRun
  where
    runTests =
      defaultMain $
        testGroup
          "Unit Tests"
          [ Unit.Notify.tests,
            Unit.NotifySystem.tests
          ]

    dontRun = putStrLn "*** Unit tests disabled. Enable with TEST_UNIT=1 ***"
