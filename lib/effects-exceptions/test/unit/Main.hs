{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (when, zipWithM_)
import Data.List qualified as L
#if WINDOWS
import Data.Text qualified as T
#endif
import Data.Proxy (Proxy (Proxy))
import Effects.Exception
  ( Exception (displayException, fromException, toException),
    ExceptionCS (MkExceptionCS),
    ExceptionProxy (MkExceptionProxy),
    SomeException,
    addCS,
    displayException,
    displayNoCS,
    displayNoCSIfMatch,
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
        displaysNoCallStackNested,
        displaysNoCSForSingleMatch,
        displaysNoCSForLaterMatch,
        displaysNoCSForMultiMatch,
        displaysCSForNoSingleMatch,
        displaysCSForNoMultiMatch
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
#if MIN_VERSION_base(4, 18, 0)
        [ "ExitFailure 0",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitWith, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitFailure, called at test/unit/Main.hs:0:0 in main:Main"
        ]
#else
        [ "ExitFailure 0",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception"
        ]
#endif

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

data ExA = MkExA
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExB = MkExB
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExC = MkExC
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

displaysNoCSForSingleMatch :: (HasCallStack) => TestTree
displaysNoCSForSingleMatch = testCase "Does not display callstack for single match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e -> "MkExB" @=? displayNoCSIfMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysNoCSForLaterMatch :: (HasCallStack) => TestTree
displaysNoCSForLaterMatch = testCase "Does not display callstack for later match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? displayNoCSIfMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysNoCSForMultiMatch :: (HasCallStack) => TestTree
displaysNoCSForMultiMatch = testCase "Does not display callstack for match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? displayNoCSIfMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExC),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysCSForNoSingleMatch :: (HasCallStack) => TestTree
displaysCSForNoSingleMatch = testCase "Dipslays callstack for no single match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> assertResults expected (L.lines $ sanitize $ displayNoCSIfMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]
    expected =
      fmap
        portPaths
        [ "MkExC",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  displaysCSForNoSingleMatch, called at test/unit/Main.hs:0:0 in main:Main"
        ]

displaysCSForNoMultiMatch :: (HasCallStack) => TestTree
displaysCSForNoMultiMatch = testCase "Dipslays callstack for no multi match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e -> assertResults expected (L.lines $ sanitize $ displayNoCSIfMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]
    expected =
      fmap
        portPaths
        [ "MkExB",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Main.hs:0:0 in main:Main",
          "  displaysCSForNoMultiMatch, called at test/unit/Main.hs:0:0 in main:Main"
        ]

show' :: (Show a) => a -> String
show' = zeroNums . show

displayException' :: (Exception e) => e -> String
displayException' = sanitize . displayException

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

sanitize :: String -> String
sanitize = stripPkgName . zeroNums

-- crude, but it works
stripPkgName :: String -> String
stripPkgName [] = []
stripPkgName (L.stripPrefix "effects-exceptions-0.0-" -> Just rest) =
  "effects-exceptions-0.0-<pkg>" ++ stripPkgName (skipUntilColon rest)
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
          ").\n\nExpected:\n",
          show expected,
          "\n\nResults:\n",
          show results
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
