{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,20,0)

{-# LANGUAGE ViewPatterns #-}

module Annotation.Utils (tests) where

import Control.Exception
  ( ExceptionWithContext,
    SomeException (SomeException),
    addExceptionContext,
    someExceptionContext,
  )
import Control.Exception.Backtrace (collectBacktraces)
import Control.Exception.Context (displayExceptionContext)
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
  ( Exception (displayException, toException),
    ExceptionProxy (MkExceptionProxy),
    ExitCode (ExitFailure, ExitSuccess),
    displayException,
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
import Text.Read qualified as TR

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
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "ExitFailure 0",
          "HasCallStack backtrace:",
          "  throwM, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  throwCS, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitWith, called at src/Effects/Exception.hs:0:0 in effects-exceptions-0.0-<pkg>:Effects.Exception",
          "  exitFailure, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
        ]

throwsExitSuccess :: TestTree
throwsExitSuccess =
  testCase "Calls exitSuccess" $
    tryAny exitSuccess >>= \case
      Left e -> assertResults expected (L.lines $ displayException' e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "ExitSuccess",
          "HasCallStack backtrace:",
          "  collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:0:0 in ghc-internal:GHC.Internal.Exception",
          "  toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/IO.hs:0:0 in ghc-internal:GHC.Internal.IO",
          "  throwIO, called at libraries/ghc-internal/src/GHC/Internal/System/Exit.hs:0:0 in ghc-internal:GHC.Internal.System.Exit",
          ""
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
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "MkEx",
          "HasCallStack backtrace:",
          "  throwM, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  catchesContext, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  catchTests, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
        ]

catchesOriginal :: (HasCallStack) => TestTree
catchesOriginal = testCase "catches exception without stacktrace" $ do
  try @_ @Ex (throwM MkEx) >>= \case
    Left e -> assertResults expected (L.lines $ displayException' e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected = ["MkEx"]

-- Notice this does not include MkEx in the callstack :-(
catchesGetsContext :: (HasCallStack) => TestTree
catchesGetsContext = testCase "catches exception and gets context" $ do
  tryAny (throwM MkEx) >>= \case
    Left e ->
      let context = someExceptionContext e
       in assertResults expected (L.lines $ zeroNums $ displayExceptionContext context)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    expected =
      fmap
        portPaths
        [ "HasCallStack backtrace:",
          "  throwM, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  catchesGetsContext, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  catchTests, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "",
          ""
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
    Left e -> assertResults expected (L.lines $ sanitize $ displayInnerMatch matches e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    matches = [MkExceptionProxy (Proxy @ExB)]
    expected =
      fmap
        portPaths
        [ "MkExC",
          "HasCallStack backtrace:",
          "  throwM, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  displaysOuterForNoSingleMatch, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
        ]

displaysOuterForNoMultiMatch :: (HasCallStack) => TestTree
displaysOuterForNoMultiMatch = testCase "Displays callstack for no multi match" $ do
  tryAny (throwM MkExB) >>= \case
    Left e -> assertResults expected (L.lines $ sanitize $ displayInnerMatch matches e)
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
          "HasCallStack backtrace:",
          "  throwM, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  displaysOuterForNoMultiMatch, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
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
  assertResults expected (L.lines $ sanitize str)
  where
    expected =
      fmap
        portPaths
        [ "MkExA",
          "HasCallStack backtrace:",
          "  collectBacktraces, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  displayNoCSIfMatchHandlerDefault, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
        ]

displayNoCSIfMatchHandlerNoMatches :: (HasCallStack) => TestTree
displayNoCSIfMatchHandlerNoMatches = testCase "Displays callstack by no proxy matches" $ do
  backtraces <- collectBacktraces
  let ex = addExceptionContext backtraces (toException MkExA)
  str <- runDisplayNoCSIfMatchHandler proxies ex
  assertResults expected (L.lines $ sanitize str)
  where
    expected =
      fmap
        portPaths
        [ "MkExA",
          "HasCallStack backtrace:",
          "  collectBacktraces, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  displayNoCSIfMatchHandlerNoMatches, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
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
  assertResults expected (L.lines $ sanitize str)
  where
    expected =
      fmap
        portPaths
        [ "ExitFailure 0",
          "HasCallStack backtrace:",
          "  collectBacktraces, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          "  displayNoCSIfMatchHandlerExitFailure, called at test/unit/Annotation/Utils.hs:0:0 in main:Annotation.Utils",
          ""
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

#else

module Annotation.Utils (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Effects.Exceptions.Annotation.Utils" []

#endif
