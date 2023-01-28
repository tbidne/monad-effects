module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.Functor (($>), (<&>))
import Data.String (IsString (fromString))
import Effects.Exception
  ( Exception (..),
    ExceptionCS (MkExceptionCS),
    HasCallStack,
    SomeException,
    addCS,
    catchCS,
    displayException,
    displayNoCS,
    throwCS,
    throwM,
    tryAny,
  )
import GHC.Stack (callStack)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Text.Read qualified as TR

main :: IO ()
main =
  defaultMain $
    testGroup
      "CallStack Tests"
      [ throwsCallStack,
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

throwsCallStack :: HasCallStack => TestTree
throwsCallStack =
  goldenVsStringDiff desc diff gpath $
    tryAny (throwCS MkEx) <&> \case
      Left e -> displayExceptionBS e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Throws with callstack"
    gpath = goldenPath </> "throw-callstack.golden"

catchTests :: HasCallStack => TestTree
catchTests =
  testGroup
    "Catching"
    [ catchesCallStackWrapped,
      catchesCallStackOriginal,
      catchesCallStackAny
    ]

catchesCallStackWrapped :: HasCallStack => TestTree
catchesCallStackWrapped = goldenVsStringDiff desc diff gpath $ do
  (throwCS MkEx $> "Error: did not catch expected exception.")
    `catchCS` \(e :: ExceptionCS Ex) ->
      pure $ displayExceptionBS e
  where
    desc = "catchCS catches wrapped exception"
    gpath = goldenPath </> "catches-callstack-wrapped.golden"

catchesCallStackOriginal :: HasCallStack => TestTree
catchesCallStackOriginal = goldenVsStringDiff desc diff gpath $ do
  (throwCS MkEx $> "Error: did not catch expected exception.")
    `catchCS` \(e :: Ex) ->
      pure $ displayExceptionBS e
  where
    desc = "catchCS catches the original exception"
    gpath = goldenPath </> "catches-callstack-original.golden"

catchesCallStackAny :: HasCallStack => TestTree
catchesCallStackAny = goldenVsStringDiff desc diff gpath $ do
  (throwCS MkEx $> "Error: did not catch expected exception.")
    `catchCS` \(e :: ExceptionCS SomeException) ->
      pure $ displayExceptionBS e
  where
    desc = "catchCS catches any exception"
    gpath = goldenPath </> "catches-callstack-any.golden"

toExceptionTests :: HasCallStack => TestTree
toExceptionTests =
  testGroup
    "toException"
    [ toExceptionBasic,
      toExceptionNested
    ]

toExceptionBasic :: HasCallStack => TestTree
toExceptionBasic = goldenVsStringDiff desc diff gpath $ do
  pure $ displayExceptionBS $ toException ex
  where
    ex = MkExceptionCS MkEx callStack
    desc = "Converts basic"
    gpath = goldenPath </> "toException.golden"

toExceptionNested :: HasCallStack => TestTree
toExceptionNested = goldenVsStringDiff desc diff gpath $ do
  pure $ displayExceptionBS $ toException ex
  where
    ex = MkExceptionCS (MkExceptionCS MkEx callStack) callStack
    desc = "Flattens nested"
    gpath = goldenPath </> "toException-nested.golden"

fromExceptionTests :: HasCallStack => TestTree
fromExceptionTests =
  testGroup
    "fromException"
    [ fromExceptionWrapped,
      fromExceptionWrappedNested,
      fromExceptionDirect
    ]

fromExceptionWrapped :: HasCallStack => TestTree
fromExceptionWrapped = goldenVsStringDiff desc diff gpath $ do
  pure $ showBS $ fromException @(ExceptionCS Ex) ex
  where
    ex = toException $ MkExceptionCS MkEx callStack
    desc = "Converts to (ExceptionCS Ex)"
    gpath = goldenPath </> "fromException-wrapped.golden"

fromExceptionWrappedNested :: HasCallStack => TestTree
fromExceptionWrappedNested = goldenVsStringDiff desc diff gpath $ do
  pure $ showBS $ fromException @(ExceptionCS Ex) ex
  where
    ex = toException $ MkExceptionCS (toException MkEx) callStack
    desc = "Converts nested to (ExceptionCS Ex)"
    gpath = goldenPath </> "fromException-wrapped-nested.golden"

fromExceptionDirect :: TestTree
fromExceptionDirect = goldenVsStringDiff desc diff gpath $ do
  pure $ showBS $ fromException @(ExceptionCS Ex) ex
  where
    ex = toException MkEx
    desc = "Converts to Ex"
    gpath = goldenPath </> "fromException-direct.golden"

addsCallStack :: HasCallStack => TestTree
addsCallStack =
  goldenVsStringDiff desc diff gpath $
    tryAny (addCS $ throwM MkEx) <&> \case
      Left e -> displayExceptionBS e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Adds callstack"
    gpath = goldenPath </> "add-callstack.golden"

addsCallStackMerges :: HasCallStack => TestTree
addsCallStackMerges =
  goldenVsStringDiff desc diff gpath $
    tryAny (addCS $ throwCS MkEx) <&> \case
      Left e -> displayExceptionBS e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Adds callstack merges callstacks"
    gpath = goldenPath </> "add-callstack-merges.golden"

displaysNoCallStack :: HasCallStack => TestTree
displaysNoCallStack =
  goldenVsStringDiff desc diff gpath $
    tryAny (throwCS MkEx) <&> \case
      Left e -> fromString $ displayNoCS e
      Right _ -> "Error: did not catch expected exception."
  where
    desc = "Does not display callstack"
    gpath = goldenPath </> "no-callstack.golden"

displaysNoCallStackNested :: HasCallStack => TestTree
displaysNoCallStackNested =
  goldenVsStringDiff desc diff gpath $
    pure $
      fromString $
        displayNoCS ex
  where
    ex =
      MkExceptionCS
        ( MkExceptionCS
            (MkExceptionCS MkEx callStack)
            callStack
        )
        callStack
    desc = "Does not display nested callstack"
    gpath = goldenPath </> "no-callstack-nested.golden"

goldenPath :: FilePath
goldenPath = "test/unit/"

diff :: FilePath -> FilePath -> [FilePath]
diff ref new = ["diff", "-u", ref, new]

showBS :: Show a => a -> BSL.ByteString
showBS = fromString . zeroNums . show

displayExceptionBS :: Exception e => e -> BSL.ByteString
displayExceptionBS = fromString . zeroNums . displayException

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
