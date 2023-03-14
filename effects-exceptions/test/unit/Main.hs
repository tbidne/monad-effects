{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (when, zipWithM_)
import Data.List qualified as L
#if WINDOWS
import Data.Text qualified as T
#endif
import Effects.Exception
  ( Exception (..),
    ExceptionCS (MkExceptionCS),
    SomeException,
    addCS,
    displayException,
    displayNoCS,
    exitFailure,
    throwCS,
    throwM,
    tryAny,
    tryCS,
  )
import GHC.Stack (callStack)
import System.Exit (exitSuccess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import Text.Read qualified as TR

main :: IO ()
main =
  defaultMain $
    testGroup
      "CallStack Tests"
      [ throwsCallStack,
        throwsExitFailure,
        throwsExitSuccess,
        catchTests,
        toExceptionTests,
        addsCallStack,
        addsCallStackMerges,
        fromExceptionTests,
        displaysNoCallStack,
        displaysNoCallStackNested
      ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsCallStack :: (HasCallStack) => TestTree
throwsCallStack = testCase "Throws with callstack" $ do
  tryAny (throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  throwsCallStack, called at test/unit/Main.hs:0:0 in main:Main"
        ]

throwsExitFailure :: TestTree
throwsExitFailure = testCase "Calls exitFailure" $ do
  tryAny exitFailure >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "ExitFailure 0",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception"
        ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    tryAny exitSuccess >>= \case
      Left e -> assertResults expected (L.lines $ displayException' e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["ExitSuccess"]

catchTests :: (HasCallStack) => TestTree
catchTests =
  testGroup
    "Catching"
    [ catchesCallStackWrapped,
      catchesCallStackOriginal,
      catchesCallStackAny
    ]

catchesCallStackWrapped :: (HasCallStack) => TestTree
catchesCallStackWrapped = testCase "catchCS catches wrapped exception" $ do
  tryCS @_ @(ExceptionCS Ex) (throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  catchesCallStackWrapped, called at test/unit/Main.hs:0:0 in main:Main",
          "  catchTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

catchesCallStackOriginal :: (HasCallStack) => TestTree
catchesCallStackOriginal = testCase "catchCS catches the original exception" $ do
  tryCS @_ @Ex (throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["MkEx"]

catchesCallStackAny :: (HasCallStack) => TestTree
catchesCallStackAny = testCase "catchCS catches any exception" $ do
  tryCS @_ @(ExceptionCS SomeException) (throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  catchesCallStackAny, called at test/unit/Main.hs:0:0 in main:Main",
          "  catchTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

toExceptionTests :: (HasCallStack) => TestTree
toExceptionTests =
  testGroup
    "toException"
    [ toExceptionBasic,
      toExceptionNested
    ]

toExceptionBasic :: (HasCallStack) => TestTree
toExceptionBasic =
  testCase "Converts basic" $
    assertResults expected (L.lines $ displayException' $ toException ex)
  where
    ex = MkExceptionCS MkEx callStack
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  toExceptionBasic, called at test/unit/Main.hs:0:0 in main:Main",
          "  toExceptionTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

toExceptionNested :: (HasCallStack) => TestTree
toExceptionNested =
  testCase "Flattens nested" $
    assertResults expected (L.lines $ displayException' $ toException ex)
  where
    ex = MkExceptionCS (MkExceptionCS MkEx callStack) callStack
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  toExceptionNested, called at test/unit/Main.hs:0:0 in main:Main",
          "  toExceptionTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

fromExceptionTests :: (HasCallStack) => TestTree
fromExceptionTests =
  testGroup
    "fromException"
    [ fromExceptionWrapped,
      fromExceptionWrappedNested,
      fromExceptionDirect
    ]

fromExceptionWrapped :: (HasCallStack) => TestTree
fromExceptionWrapped =
  testCase "Flattens nested" $
    assertResults expected (L.lines $ show' $ fromException @(ExceptionCS Ex) ex)
  where
    ex = toException $ MkExceptionCS MkEx callStack
    expected =
      fmap
        portPaths
        [ "Just MkEx",
          "CallStack (from HasCallStack):",
          "  fromExceptionWrapped, called at test/unit/Main.hs:0:0 in main:Main",
          "  fromExceptionTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

fromExceptionWrappedNested :: (HasCallStack) => TestTree
fromExceptionWrappedNested =
  testCase "Converts nested to (ExceptionCS Ex)" $
    assertResults expected (L.lines $ show' $ fromException @(ExceptionCS Ex) ex)
  where
    ex = toException $ MkExceptionCS (toException MkEx) callStack
    expected =
      fmap
        portPaths
        [ "Just MkEx",
          "CallStack (from HasCallStack):",
          "  fromExceptionWrappedNested, called at test/unit/Main.hs:0:0 in main:Main",
          "  fromExceptionTests, called at test/unit/Main.hs:0:0 in main:Main"
        ]

fromExceptionDirect :: TestTree
fromExceptionDirect =
  testCase "Converts to Ex" $
    "Just MkEx\n" @=? show' (fromException @(ExceptionCS Ex) ex)
  where
    ex = toException MkEx

addsCallStack :: (HasCallStack) => TestTree
addsCallStack = testCase "Adds callstack" $ do
  tryAny (addCS $ throwM MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  addCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  addsCallStack, called at test/unit/Main.hs:0:0 in main:Main"
        ]

addsCallStackMerges :: (HasCallStack) => TestTree
addsCallStackMerges = testCase "Adds callstack merges callstacks" $ do
  tryAny (addCS $ throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  addsCallStackMerges, called at test/unit/Main.hs:0:0 in main:Main",
          "  addCS, called at test/unit/Main.hs:0:0 in main:Main"
        ]

displaysNoCallStack :: (HasCallStack) => TestTree
displaysNoCallStack = testCase "Does not display callstack" $ do
  tryAny (throwCS MkEx) >>= \case
    Left e -> "MkEx" @=? displayNoCS e
    Right _ -> assertFailure "Error: did not catch expected exception."

displaysNoCallStackNested :: (HasCallStack) => TestTree
displaysNoCallStackNested = testCase "Does not display nested callstack" $ do
  "MkEx" @=? displayNoCS ex
  where
    ex =
      MkExceptionCS
        ( MkExceptionCS
            (MkExceptionCS MkEx callStack)
            callStack
        )
        callStack

show' :: (Show a) => a -> String
show' = zeroNums . show

displayException' :: (Exception e) => e -> String
displayException' = stripPkgName . zeroNums . displayException

zeroNums :: String -> String
zeroNums [] = []
zeroNums (x : xs) = case TR.readMaybe @Int [x] of
  Nothing -> x : zeroNums xs
  Just _ -> '0' : zeroNums (skipNums xs)
  where
    skipNums [] = []
    skipNums (y : ys) = case TR.readMaybe @Int [y] of
      Nothing -> y : ys
      Just _ -> skipNums ys

-- crude, but it works
stripPkgName :: String -> String
stripPkgName [] = []
stripPkgName (L.stripPrefix "effects-exceptions-0.0-" -> Just rest) =
  "effects-exceptions-0.0-<pkg>" ++ skipUntilColon rest
stripPkgName (x : xs) = x : stripPkgName xs

skipUntilColon :: String -> String
skipUntilColon [] = []
skipUntilColon (':' : rest) = ':' : rest
skipUntilColon (_ : xs) = skipUntilColon xs

assertResults :: (Eq a, Show a) => [a] -> [a] -> IO ()
assertResults expected results = do
  when (lenExpected /= lenResults) $
    assertFailure $
      mconcat
        [ "Expected length (",
          show lenExpected,
          ") did not match results length (",
          show lenResults,
          ")."
        ]
  zipWithM_ (@=?) expected results
  where
    lenExpected = length expected
    lenResults = length results

portPaths :: String -> String
#if WINDOWS && GHC_9_4
portPaths = T.unpack . (T.replace "/" "\\") . T.pack
#elif WINDOWS
portPaths = T.unpack . (T.replace "/" "\\\\") . T.pack
#else
portPaths = id
#endif
