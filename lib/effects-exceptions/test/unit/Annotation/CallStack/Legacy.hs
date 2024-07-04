{-# LANGUAGE CPP #-}

module Annotation.CallStack.Legacy (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
#if MIN_VERSION_base(4,20,0)
import Effects.Exception.Annotation.CallStack.Legacy
  ( ExceptionCS (MkExceptionCS),
    addCS,
    displayCSNoMatch,
    displayCSNoMatchHandler,
    displayNoCS,
    throwCS,
    tryCS,
  )
import Effects.Exception
  ( Exception (fromException, toException),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    SomeException,
    exitFailure,
    throwM,
    tryAny,
  )
#else
import Effects.Exception
  ( Exception (fromException, toException),
    ExceptionCS (MkExceptionCS),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    SomeException,
    addCS,
    displayCSNoMatch,
    displayCSNoMatchHandler,
    displayNoCS,
    exitFailure,
    throwCS,
    throwM,
    tryAny,
    tryCS,
  )
#endif
import GHC.Stack (callStack)
import System.Exit (exitSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import TestUtils qualified

tests :: TestTree
tests =
  testGroup
    "Effects.Exceptions.CallStack.Legacy"
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
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "CallStack",
        "  throwCS",
        "  throwsCallStack"
      ]

throwsExitFailure :: TestTree
throwsExitFailure = testCase "Calls exitFailure" $ do
  tryAny exitFailure >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "ExitFailure 1",
        "CallStack",
        "  throwCS",
        "  exitWith",
        "  exitFailure"
      ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    tryAny exitSuccess >>= \case
      Left e ->
        TestUtils.assertContainsMinLines 1 expected (TestUtils.displayExceptiont e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    -- Base >= 4.20 includes a callstack, but < 4.20 does not, hence we need
    -- to be pessimistic.
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
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "CallStack",
        "  throwCS",
        "  catchesCallStackWrapped",
        "  catchTests"
      ]

catchesCallStackOriginal :: (HasCallStack) => TestTree
catchesCallStackOriginal = testCase "catchCS catches the original exception" $ do
  tryCS @_ @Ex (throwCS MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 1 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["MkEx"]

catchesCallStackAny :: (HasCallStack) => TestTree
catchesCallStackAny = testCase "catchCS catches any exception" $ do
  tryCS @_ @(ExceptionCS SomeException) (throwCS MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "CallStack",
        "  throwCS",
        "  catchesCallStackAny",
        "  catchTests"
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
    TestUtils.assertContainsMinLines
      4
      expected
      (TestUtils.displayExceptiont $ toException ex)
  where
    ex = MkExceptionCS MkEx callStack
    expected =
      [ "MkEx",
        "CallStack (from HasCallStack):",
        "  toExceptionBasic",
        "  toExceptionTests"
      ]

toExceptionNested :: (HasCallStack) => TestTree
toExceptionNested =
  testCase "Flattens nested" $
    TestUtils.assertContainsMinLines
      4
      expected
      (TestUtils.displayExceptiont $ toException ex)
  where
    ex = MkExceptionCS (MkExceptionCS MkEx callStack) callStack
    expected =
      [ "MkEx",
        "CallStack (from HasCallStack):",
        "  toExceptionNested",
        "  toExceptionTests"
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
    TestUtils.assertContainsMinLines
      4
      expected
      (TestUtils.showt $ fromException @(ExceptionCS Ex) ex)
  where
    ex = toException $ MkExceptionCS MkEx callStack
    expected =
      [ "Just MkEx",
        "CallStack (from HasCallStack):",
        "  fromExceptionWrapped",
        "  fromExceptionTests"
      ]

fromExceptionWrappedNested :: (HasCallStack) => TestTree
fromExceptionWrappedNested =
  testCase "Converts nested to (ExceptionCS Ex)" $
    TestUtils.assertContainsMinLines
      4
      expected
      (TestUtils.showt $ fromException @(ExceptionCS Ex) ex)
  where
    ex = toException $ MkExceptionCS (toException MkEx) callStack
    expected =
      [ "Just MkEx",
        "CallStack (from HasCallStack):",
        "  fromExceptionWrappedNested",
        "  fromExceptionTests"
      ]

fromExceptionDirect :: TestTree
fromExceptionDirect =
  testCase "Converts to Ex" $
    TestUtils.assertContainsMinLines
      1
      expected
      (TestUtils.showt $ fromException @(ExceptionCS Ex) ex)
  where
    ex = toException MkEx
    expected = ["Just MkEx"]

addsCallStack :: (HasCallStack) => TestTree
addsCallStack = testCase "Adds callstack" $ do
  tryAny (addCS $ throwM MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "CallStack (from HasCallStack):",
        "  addCS, called at",
        "  addCS, called at",
        "  addsCallStack, called at"
      ]

addsCallStackMerges :: (HasCallStack) => TestTree
addsCallStackMerges = testCase "Adds callstack merges callstacks" $ do
  tryAny (addCS $ throwCS MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 7 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "CallStack (from HasCallStack):",
        "  throwCS, called at",
        "  throwCS, called at",
        "  addsCallStackMerges, called at",
        "  addCS, called at",
        "  addCS, called at"
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
    Left e -> "MkEx" @=? TestUtils.strip (displayNoCS e)
    Right _ -> assertFailure "Error: did not catch expected exception."

displaysNoCallStackNested :: (HasCallStack) => TestTree
displaysNoCallStackNested = testCase "Does not display nested callstack" $ do
  "MkEx" @=? TestUtils.strip (displayNoCS ex)
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
    Left e -> "MkExB" @=? TestUtils.strip (displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysNoCSForLaterMatch :: (HasCallStack) => TestTree
displaysNoCSForLaterMatch = testCase "Does not display callstack for later match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? TestUtils.strip (displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysNoCSForMultiMatch :: (HasCallStack) => TestTree
displaysNoCSForMultiMatch = testCase "Does not display callstack for match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? TestUtils.strip (displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExC),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysCSForNoSingleMatch :: (HasCallStack) => TestTree
displaysCSForNoSingleMatch = testCase "Displays callstack for no single match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]
    expected =
      [ "MkExC",
        "CallStack (from HasCallStack):",
        "  throwCS, called at",
        "  throwCS, called at",
        "  displaysCSForNoSingleMatch, called at"
      ]

displaysCSForNoMultiMatch :: (HasCallStack) => TestTree
displaysCSForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]
    expected =
      [ "MkExB",
        "CallStack (from HasCallStack):",
        "  throwCS, called at",
        "  throwCS, called at",
        "  displaysCSForNoMultiMatch, called at"
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
  TestUtils.assertContainsMinLines 3 expected (T.pack str)
  where
    ex = toException (MkExceptionCS MkExA callStack)
    expected =
      [ "MkExA",
        "CallStack (from HasCallStack):",
        "  displayNoCSIfMatchHandlerDefault, called at"
      ]

displayNoCSIfMatchHandlerNoMatches :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoMatches = testCase "Displays callstack by no proxy matches" $ do
  str <- runDisplayNoCSIfMatchHandler proxies ex
  TestUtils.assertContainsMinLines 3 expected (T.pack str)
  where
    ex = toException (MkExceptionCS MkExA callStack)
    expected =
      [ "MkExA",
        "CallStack (from HasCallStack):",
        "  displayNoCSIfMatchHandlerNoMatches, called at"
      ]
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerSkipsMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsMatch = testCase "Does not display callstack for match" $ do
  str <- runDisplayNoCSIfMatchHandler [MkExceptionProxy $ Proxy @ExB] ex
  "MkExB" @=? TestUtils.strip str
  where
    ex = toException MkExB

displayNoCSIfMatchHandlerSkipsCSMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsCSMatch = testCase "Does not display callstack for cs match" $ do
  str <- runDisplayNoCSIfMatchHandler proxies ex
  "MkExB" @=? TestUtils.strip str
  where
    ex = toException (MkExceptionCS MkExB callStack)
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerExitFailure :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerExitFailure = testCase "Displays callstack for ExitFailure" $ do
  str <- runDisplayNoCSIfMatchHandler [] ex
  TestUtils.assertContainsMinLines 3 expected (T.pack str)
  where
    ex = toException (MkExceptionCS (ExitFailure 1) callStack)
    expected =
      [ "ExitFailure 1",
        "CallStack (from HasCallStack):",
        "  displayNoCSIfMatchHandlerExitFailure, called at"
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
