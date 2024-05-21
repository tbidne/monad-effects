{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Annotation.CallStack.Legacy (tests) where

import Control.Monad (when, zipWithM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as L
#if WINDOWS || MIN_VERSION_base(4, 20, 0)
import Data.Text qualified as T
#endif
import Data.Proxy (Proxy (Proxy))
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
  ( Exception (displayException, fromException, toException),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    SomeException,
    exitFailure,
    throwM,
    tryAny,
  )
#else
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
#endif
import GHC.Stack (callStack)
import System.Exit (exitSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))
import Text.Read qualified as TR

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
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
#if MIN_VERSION_base(4,20,0)
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  throwsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "HasCallStack backtrace:",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  throwsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          ""
        ]
#else
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  throwsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

throwsExitFailure :: TestTree
throwsExitFailure = testCase "Calls exitFailure" $ do
  tryAny exitFailure >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
#if MIN_VERSION_base(4, 20, 0)
        [ "ExitFailure 0",
          "HasCallStack backtrace:",
          "  throwM, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitWith, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitFailure, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#else
        [ "ExitFailure 0",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitWith, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitFailure, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    tryAny exitSuccess >>= \case
      Left e -> assertResults expected (L.lines $ displayException' e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
#if MIN_VERSION_base(4,20,0)
    expected =
      fmap
        portPaths
        [ "ExitSuccess",
          "HasCallStack backtrace:",
          "  collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:0:0 in ghc-internal:GHC.Internal.Exception",
          "  toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/IO.hs:0:0 in ghc-internal:GHC.Internal.IO",
          "  throwIO, called at libraries/ghc-internal/src/GHC/Internal/System/Exit.hs:0:0 in ghc-internal:GHC.Internal.System.Exit"
        ]
#else
    expected = ["ExitSuccess"]
#endif

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
#if MIN_VERSION_base(4,20,0)
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchesCallStackWrapped, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#else
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchesCallStackWrapped, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

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
#if MIN_VERSION_base(4,20,0)
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchesCallStackAny, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#else
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchesCallStackAny, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  catchTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

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
          "  toExceptionBasic, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  toExceptionTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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
          "  toExceptionNested, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  toExceptionTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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
          "  fromExceptionWrapped, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  fromExceptionTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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
          "  fromExceptionWrappedNested, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  fromExceptionTests, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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
#if MIN_VERSION_base(4,20,0)
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "HasCallStack backtrace:",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          ""
        ]
#else
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  addCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStack, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

addsCallStackMerges :: (HasCallStack) => TestTree
addsCallStackMerges = testCase "Adds callstack merges callstacks" $ do
  tryAny (addCS $ throwCS MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
#if MIN_VERSION_base(4,20,0)
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStackMerges, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "HasCallStack backtrace:",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStackMerges, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          ""
        ]
#else
        [ "MkEx",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addsCallStackMerges, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  addCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  addCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

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
    Left e -> "MkEx" @=? strip (displayNoCS e)
    Right _ -> assertFailure "Error: did not catch expected exception."

displaysNoCallStackNested :: (HasCallStack) => TestTree
displaysNoCallStackNested = testCase "Does not display nested callstack" $ do
  "MkEx" @=? strip (displayNoCS ex)
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
    Left e -> "MkExB" @=? strip (displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]

displaysNoCSForLaterMatch :: (HasCallStack) => TestTree
displaysNoCSForLaterMatch = testCase "Does not display callstack for later match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? strip (displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]

displaysNoCSForMultiMatch :: (HasCallStack) => TestTree
displaysNoCSForMultiMatch = testCase "Does not display callstack for match" $ do
  tryAny (throwCS MkExC) >>= \case
    Left e -> "MkExC" @=? strip (displayCSNoMatch matches e)
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
#if MIN_VERSION_base(4,20,0)
        [ "MkExC",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoSingleMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy","HasCallStack backtrace:",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoSingleMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          ""
        ]
#else
        [ "MkExC",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoSingleMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

displaysCSForNoMultiMatch :: (HasCallStack) => TestTree
displaysCSForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  tryAny (throwCS MkExB) >>= \case
    Left e -> assertResults expected (removeEmpty $ L.lines $ sanitize $ displayCSNoMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches =
      [ MkExceptionProxy (Proxy @ExA),
        MkExceptionProxy (Proxy @ExC)
      ]
    expected =
      fmap
        portPaths
#if MIN_VERSION_base(4,20,0)
        [ "MkExB",
          "CallStack (from HasCallStack):",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoMultiMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy","HasCallStack backtrace:",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoMultiMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#else
        [ "MkExB",
          "CallStack (from HasCallStack):",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy",
          "  displaysCSForNoMultiMatch, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
#endif

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
          "  displayNoCSIfMatchHandlerDefault, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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
          "  displayNoCSIfMatchHandlerNoMatches, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
        ]
    proxies =
      [ MkExceptionProxy $ Proxy @ExB,
        MkExceptionProxy $ Proxy @ExC
      ]

displayNoCSIfMatchHandlerSkipsMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsMatch = testCase "Does not display callstack for match" $ do
  str <- runDisplayNoCSIfMatchHandler [MkExceptionProxy $ Proxy @ExB] ex
  "MkExB" @=? strip str
  where
    ex = toException MkExB

displayNoCSIfMatchHandlerSkipsCSMatch :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerSkipsCSMatch = testCase "Does not display callstack for cs match" $ do
  str <- runDisplayNoCSIfMatchHandler proxies ex
  "MkExB" @=? strip str
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
          "  displayNoCSIfMatchHandlerExitFailure, called at test/unit/Annotation/CallStack/Legacy.hs:0:0 in main:Annotation.CallStack.Legacy"
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

assertResults :: [String] -> [String] -> IO ()
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
          show results'
        ]
  zipWithM_ (@=?) expected results'
  where
    lenExpected = length expected
    lenResults = length results'

    results' = removeEmpty results

portPaths :: String -> String
#if WINDOWS && GHC_LT_9_4
portPaths = T.unpack . replaceSlashes "\\\\" . T.pack
#elif WINDOWS
portPaths = T.unpack . replaceSlashes "\\" . T.pack
#else
portPaths = id
#endif

#if WINDOWS
-- For reasons I do not understand, in callstack exception messages we receive
-- windows paths like test/foo\\bar\\.
--
-- Why isn't the test/ slash a backslash? Who knows? In any case, replace
-- everything but that.
replaceSlashes :: T.Text -> T.Text -> T.Text
replaceSlashes slashes t = case T.breakOnEnd "test/" t of
  ("", e) -> T.replace "/" slashes e
  (s, e) -> s <> T.replace "/" slashes e
#endif

-- NOTE: [Base 4.20+ Output Differences]
--
-- The output changes with base 4.20. The main change is that SomeException's
-- displayException now prints annotations by default. We don't _really_
-- care about the differences since anyone on 4.20+ should be using the
-- new annotations and not the Legacy module. Nevertheless, we still want
-- to assert that the basic functionality still works.
--
-- To do this, we use CPP to expect different output where necessary, and
-- provide helper functions below.

-- Remove empty strings. base 4.20's SomeException instance adds newlines.
removeEmpty :: [String] -> [String]
-- Strip whitespace. base 4.20's SomeException instance adds newlines.
strip :: String -> String

#if MIN_VERSION_base(4,20,0)

strip = T.unpack . T.strip . T.pack

removeEmpty [] = []
removeEmpty ("" : xs) = xs
removeEmpty (x : xs) = x : removeEmpty xs

#else

removeEmpty = id
strip = id

#endif
