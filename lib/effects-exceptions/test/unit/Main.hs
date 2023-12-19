{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Monad (when, zipWithM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
#if WINDOWS
import Data.Text qualified as T
#endif
import Data.Proxy (Proxy (Proxy))
import Effects.Exception
  ( Exception (displayException, fromException, toException),
    ExceptionCS (MkExceptionCS),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    SomeException,
    addCS,
    displayCSNoMatch,
    displayCSNoMatchHandler,
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
      "Effects.Exceptions"
      [ throwsTests,
        catchTests,
        toExceptionTests,
        addsCallStack,
        addsCallStackMerges,
        fromExceptionTests,
        displaysCSTests,
        displayNoCSIfMatchTests,
        displayNoCSIfMatchHandlerTests
      ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsTests :: TestTree
throwsTests =
  testGroup
    "Throwing"
    [ throwsCallStack,
      throwsExitFailure,
      throwsExitSuccess
    ]

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

displaysCSTests :: TestTree
displaysCSTests =
  testGroup
    "displayNoCS"
    [ displaysNoCallStack,
      displaysNoCallStackNested
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

displayNoCSIfMatchTests :: TestTree
displayNoCSIfMatchTests =
  testGroup
    "displayCSNoMatch"
    [ displaysNoCSForSingleMatch,
      displaysNoCSForLaterMatch,
      displaysNoCSForMultiMatch,
      displaysCSForNoSingleMatch,
      displaysCSForNoMultiMatch
    ]

displaysNoCSForSingleMatch :: (HasCallStack) => TestTree
displaysNoCSForSingleMatch = testCase "Does not display callstack for single match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e -> "MkExB" @=? displayCSNoMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysNoCSForLaterMatch :: (HasCallStack) => TestTree
displaysNoCSForLaterMatch = testCase "Does not display callstack for later match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? displayCSNoMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysNoCSForMultiMatch :: (HasCallStack) => TestTree
displaysNoCSForMultiMatch = testCase "Does not display callstack for match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? displayCSNoMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExC),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysCSForNoSingleMatch :: (HasCallStack) => TestTree
displaysCSForNoSingleMatch = testCase "Displays callstack for no single match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> assertResults expected (L.lines $ sanitize $ displayCSNoMatch matches e)
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
displaysCSForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e -> assertResults expected (L.lines $ sanitize $ displayCSNoMatch matches e)
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

displayNoCSIfMatchHandlerTests :: TestTree
displayNoCSIfMatchHandlerTests =
  testGroup
    "displayCSNoMatchHandler"
    [ displayNoCSIfMatchHandlerDefault,
      displayNoCSIfMatchHandlerNoMatches,
      displayNoCSIfMatchHandlerSkipsMatch,
      displayNoCSIfMatchHandlerSkipsCSMatch,
      displayNoCSIfMatchHandlerExitFailure,
      displayNoCSIfMatchHandlerNoExitSuccess,
      displayNoCSIfMatchHandlerNoCSExitSuccess
    ]

displayNoCSIfMatchHandlerDefault :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerDefault = testCase "Displays callstack by default" $ do
  str <- runDisplayNoCSIfMatchHandler [] ex
  assertResults expected (L.lines $ sanitize str)
  where
    ex = toException (MkExceptionCS MkExA callStack)
    expected =
      fmap
        portPaths
        [ "MkExA",
          "CallStack (from HasCallStack):",
          "  displayNoCSIfMatchHandlerDefault, called at test/unit/Main.hs:0:0 in main:Main"
        ]

displayNoCSIfMatchHandlerNoMatches :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoMatches = testCase "Displays callstack by no proxy matches" $ do
  str <- runDisplayNoCSIfMatchHandler proxies ex
  assertResults expected (L.lines $ sanitize str)
  where
    ex = toException (MkExceptionCS MkExA callStack)
    expected =
      fmap
        portPaths
        [ "MkExA",
          "CallStack (from HasCallStack):",
          "  displayNoCSIfMatchHandlerNoMatches, called at test/unit/Main.hs:0:0 in main:Main"
        ]
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerSkipsMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsMatch = testCase "Does not display callstack for match" $ do
  str <- runDisplayNoCSIfMatchHandler [MkExceptionProxy $ Proxy @ExB] ex
  "MkExB" @=? str
  where
    ex = toException MkExB

displayNoCSIfMatchHandlerSkipsCSMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsCSMatch = testCase "Does not display callstack for cs match" $ do
  str <- runDisplayNoCSIfMatchHandler proxies ex
  "MkExB" @=? str
  where
    ex = toException (MkExceptionCS MkExB callStack)
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerExitFailure :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerExitFailure = testCase "Displays callstack for ExitFailure" $ do
  str <- runDisplayNoCSIfMatchHandler [] ex
  assertResults expected (L.lines $ sanitize str)
  where
    ex = toException (MkExceptionCS (ExitFailure 1) callStack)
    expected =
      fmap
        portPaths
        [ "ExitFailure 0",
          "CallStack (from HasCallStack):",
          "  displayNoCSIfMatchHandlerExitFailure, called at test/unit/Main.hs:0:0 in main:Main"
        ]

displayNoCSIfMatchHandlerNoExitSuccess :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoExitSuccess = testCase desc $ do
  str <- runDisplayNoCSIfMatchHandler [] ex
  "" @=? str
  where
    desc = "Does not display callstack for ExitSuccess"
    ex = toException ExitSuccess

displayNoCSIfMatchHandlerNoCSExitSuccess :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoCSExitSuccess = testCase desc $ do
  str <- runDisplayNoCSIfMatchHandler [] ex
  "" @=? str
  where
    desc = "Does not display callstack for CallStack ExitSuccess"
    ex = toException (MkExceptionCS ExitSuccess callStack)

runDisplayNoCSIfMatchHandler :: [ExceptionProxy] -> SomeException -> IO String
runDisplayNoCSIfMatchHandler proxies = runTestIO . testIO
  where
    testIO :: SomeException -> TestIO ()
    testIO = displayCSNoMatchHandler proxies testHandler

    runTestIO :: TestIO a -> IO String
    runTestIO m = do
      ref <- newIORef ""
      _ <- unTestIO m ref
      readIORef ref

    testHandler :: String -> TestIO ()
    testHandler str = do
      ref <- MkTestIO ask
      liftIO $ writeIORef ref str

newtype TestIO a = MkTestIO (ReaderT (IORef String) IO a)
  deriving (Applicative, Functor, Monad, MonadIO) via ReaderT (IORef String) IO

unTestIO :: TestIO a -> IORef String -> IO a
unTestIO (MkTestIO io) = runReaderT io

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
