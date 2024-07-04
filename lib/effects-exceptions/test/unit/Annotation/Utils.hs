{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,20,0)

module Annotation.Utils (tests) where

import Control.Exception
  ( ExceptionWithContext,
    SomeException (SomeException),
    addExceptionContext,
  )
import Control.Exception.Backtrace (collectBacktraces)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified as T
import Effects.Exception
  ( Exception (toException),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    displayInner,
    displayInnerMatch,
    displayInnerMatchHandler,
    exitFailure,
    throwM,
    try,
    tryAny,
  )
import System.Exit (exitSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import TestUtils qualified

tests :: TestTree
tests =
  testGroup
    "Effects.Exceptions.Annotation.Utils"
    [ throwsTests,
      catchTests,
      displayInnerTests,
      displayInnerMatchTests,
      displayNoCSIfMatchHandlerTests
    ]

data Ex = MkEx
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwsTests :: TestTree
throwsTests =
  testGroup
    "Throwing"
    [ throwsExitFailure,
      throwsExitSuccess
    ]

throwsExitFailure :: TestTree
throwsExitFailure = testCase "Calls exitFailure" $ do
  tryAny exitFailure >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 7 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "ExitFailure 1",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  throwCS, called at",
        "  exitWith, called at",
        "  exitFailure"
      ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    tryAny exitSuccess >>= \case
      Left e ->
        TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "ExitSuccess",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  toExceptionWithBacktrace, called at",
        "  throwIO, called at"
      ]

catchTests :: (HasCallStack) => TestTree
catchTests =
  testGroup
    "Catching"
    [ catchesContext,
      catchesOriginal,
      catchesGetsContext
    ]

catchesContext :: (HasCallStack) => TestTree
catchesContext = testCase "catches exception with context" $ do
  try @_ @(ExceptionWithContext Ex) (throwM MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "MkEx",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  catchesContext, called at",
        "  catchTests, called at"
      ]

catchesOriginal :: (HasCallStack) => TestTree
catchesOriginal = testCase "catches exception without stacktrace" $ do
  try @_ @Ex (throwM MkEx) >>= \case
    Left e -> TestUtils.assertContainsMinLines 1 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["MkEx"]

-- Notice this does not include MkEx in the callstack :-(
catchesGetsContext :: (HasCallStack) => TestTree
catchesGetsContext = testCase "catches exception and gets context" $ do
  tryAny (throwM MkEx) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 6 expected (TestUtils.displayExceptiont e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      [ "HasCallStack backtrace:",
        "  throwM, called at",
        "  catchesGetsContext, called at",
        "  catchTests, called at"
      ]

displayInnerTests :: TestTree
displayInnerTests =
  testGroup
    "displayInner"
    [ displaysInner,
      displaysInnerNested
    ]

displaysInner :: (HasCallStack) => TestTree
displaysInner = testCase "Displays inner exception" $ do
  tryAny (throwM MkEx) >>= \case
    Left e -> "MkEx" @=? displayInner e
    Right _ -> assertFailure "Error: did not catch expected exception."

displaysInnerNested :: (HasCallStack) => TestTree
displaysInnerNested = testCase "Displays inner exception when nested" $ do
  tryAny (throwM nestedEx) >>= \case
    Left e -> "MkEx" @=? displayInner e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    nestedEx =
      SomeException $
        SomeException $
          SomeException $
            SomeException MkEx

data ExA = MkExA
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExB = MkExB
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ExC = MkExC
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

displayInnerMatchTests :: TestTree
displayInnerMatchTests =
  testGroup
    "displayInnerMatch"
    [ displaysInnerForSingleMatch,
      displaysInnerForLaterMatch,
      displaysInnerForMultiMatch,
      displaysOuterForNoSingleMatch,
      displaysOuterForNoMultiMatch
    ]

displaysInnerForSingleMatch :: (HasCallStack) => TestTree
displaysInnerForSingleMatch = testCase "Does not display callstack for single match" $ do
  tryAny (throwM MkExB) >>= \case
    Left e -> "MkExB" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysInnerForLaterMatch :: (HasCallStack) => TestTree
displaysInnerForLaterMatch = testCase "Does not display callstack for later match" $ do
  tryAny (throwM MkExC) >>= \case
    Left e -> "MkExC" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysInnerForMultiMatch :: (HasCallStack) => TestTree
displaysInnerForMultiMatch = testCase "Does not display callstack for match" $ do
  tryAny (throwM MkExC) >>= \case
    Left e -> "MkExC" @=? displayInnerMatch matches e
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExC),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysOuterForNoSingleMatch :: (HasCallStack) => TestTree
displaysOuterForNoSingleMatch = testCase "Displays callstack for no single match" $ do
  tryAny (throwM MkExC) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayInnerMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]
    expected =
      [ "MkExC",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  displaysOuterForNoSingleMatch, called at"
      ]

displaysOuterForNoMultiMatch :: (HasCallStack) => TestTree
displaysOuterForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  tryAny (throwM MkExB) >>= \case
    Left e ->
      TestUtils.assertContainsMinLines 5 expected (T.pack $ displayInnerMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]
    expected =
      [ "MkExB",
        "HasCallStack backtrace:",
        "  throwM, called at",
        "  displaysOuterForNoMultiMatch, called at"
      ]

displayNoCSIfMatchHandlerTests :: TestTree
displayNoCSIfMatchHandlerTests =
  testGroup
    "displayInnerMatchHandler"
    [ displayNoCSIfMatchHandlerDefault,
      displayNoCSIfMatchHandlerNoMatches,
      displayNoCSIfMatchHandlerSkipsMatch,
      displayNoCSIfMatchHandlerSkipsCSMatch,
      displayNoCSIfMatchHandlerExitFailure,
      displayNoCSIfMatchHandlerNoExitSuccess
    ]

displayNoCSIfMatchHandlerDefault :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerDefault = testCase "Displays callstack by default" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExA)
  str <- runDisplayNoCSIfMatchHandler [] ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "MkExA",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerDefault, called at",
        ""
      ]

displayNoCSIfMatchHandlerNoMatches :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoMatches = testCase "Displays callstack by no proxy matches" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExA)
  str <- runDisplayNoCSIfMatchHandler proxies ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "MkExA",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerNoMatches, called at"
      ]
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerSkipsMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsMatch = testCase "Does not display callstack for match" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExB)
  str <- runDisplayNoCSIfMatchHandler [MkExceptionProxy $ Proxy @ExB] ex
  "MkExB" @=? str

displayNoCSIfMatchHandlerSkipsCSMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsCSMatch = testCase "Does not display callstack for cs match" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExB)
  str <- runDisplayNoCSIfMatchHandler proxies ex
  "MkExB" @=? str
  where
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerExitFailure :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerExitFailure = testCase "Displays callstack for ExitFailure" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException (ExitFailure 1))
  str <- runDisplayNoCSIfMatchHandler [] ex
  TestUtils.assertContainsMinLines 5 expected (T.pack str)
  where
    expected =
      [ "ExitFailure 1",
        "HasCallStack backtrace:",
        "  collectBacktraces, called at",
        "  displayNoCSIfMatchHandlerExitFailure, called at"
      ]

displayNoCSIfMatchHandlerNoExitSuccess :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoExitSuccess = testCase desc $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException ExitSuccess)
  str <- runDisplayNoCSIfMatchHandler [] ex
  "" @=? str
  where
    desc = "Does not display callstack for ExitSuccess"

runDisplayNoCSIfMatchHandler :: [ExceptionProxy] -> SomeException -> IO String
runDisplayNoCSIfMatchHandler proxies = runTestIO . testIO
  where
    testIO :: SomeException -> TestIO ()
    testIO = displayInnerMatchHandler proxies testHandler

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

#else

module Annotation.Utils (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Effects.Exceptions.Annotation.Utils" []

#endif
