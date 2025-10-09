{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.Effects.Logger (tests) where

import Control.Monad.Logger (Loc (Loc), NoLoggingT (runNoLoggingT))
import Control.Monad.Logger.CallStack (LogLevel (LevelDebug))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 qualified as Char8
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Logger
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
    LogLevel (LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    logDebug,
    logError,
    logFatal,
    logInfo,
    logOther,
    logTrace,
    logWarn,
    shouldLog,
    _LevelFatal,
    _LevelOther,
    _LevelTrace,
  )
import Effects.Logger.Namespace
  ( Namespace (MkNamespace),
    logStrToBs,
  )
import Hedgehog
  ( Gen,
    annotate,
    assert,
    failure,
    forAll,
    property,
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Optics.Core
  ( An_AffineFold,
    Is,
    Optic',
    Prism',
    preview,
    prism,
  )
import Optics.Core.Extras (is)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Effects.Logger"
    [ formatTests,
      logLevelTH,
      shouldLevelTests
    ]

formatTests :: TestTree
formatTests =
  testGroup
    "Formatting"
    [ formatBasic,
      formatBasicTrace,
      formatBasicFatal,
      formatNewline,
      formatTimezone,
      formatLocStable,
      formatLocPartial,
      formatThreadId,
      formatThreadLabel
    ]

formatBasic :: TestTree
formatBasic = testCase "Formats a basic namespaced log" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  "[2022-02-08 10:20:05][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatBasicTrace :: TestTree
formatBasicTrace = testCase "Formats a trace log" $ do
  logMsg <- runLogger (formatTrace fmt) emptyNamespace
  "[2022-02-08 10:20:05][Trace] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatBasicFatal :: TestTree
formatBasicFatal = testCase "Formats a fatal log" $ do
  logMsg <- runLogger (formatFatal fmt) emptyNamespace
  "[2022-02-08 10:20:05][Fatal] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatNewline :: TestTree
formatNewline = testCase "Formats a log with a newline" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  "[2022-02-08 10:20:05][Warn] msg\n" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = True,
          threadLabel = False,
          timezone = False
        }

formatTimezone :: TestTree
formatTimezone = testCase "Formats a log with a timezone" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  "[2022-02-08 10:20:05 UTC][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = False,
          timezone = True
        }

formatLocStable :: TestTree
formatLocStable = testCase "Formats a log with stable loc" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  "[2022-02-08 10:20:05][filename][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocStable loc,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatLocPartial :: TestTree
formatLocPartial = testCase "Formats a log with partial loc" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  "[2022-02-08 10:20:05][filename:1:2][Warn] msg" @=? fromLogStr logMsg
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocPartial loc,
          newline = False,
          threadLabel = False,
          timezone = False
        }

formatThreadId :: TestTree
formatThreadId = testCase "Formats a log with thread id" $ do
  logMsg <- runLogger (format fmt) emptyNamespace
  let logMsgStr = fromLogStr logMsg
      logMsgTxt = T.pack logMsgStr

  -- The thread id itself is non-deterministic, so we have to split the
  -- asserts up.
  assertBool logMsgStr ("[2022-02-08 10:20:05][ThreadId " `T.isPrefixOf` logMsgTxt)
  assertBool logMsgStr ("][Warn] msg" `T.isSuffixOf` logMsgTxt)
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = True,
          timezone = False
        }

{- ORMOLU_DISABLE -}

formatThreadLabel :: TestTree
formatThreadLabel = testCase "Formats a log with thread label" $ do
  logMsg <- runLoggerThreadLabel (format fmt) emptyNamespace
#if MIN_VERSION_base(4, 18, 0)
  "[2022-02-08 10:20:05][some-thread][Warn] msg"
    @=? fromLogStr logMsg
#else

  -- TODO: Remove this case once we unconditionally have base >= 4.18
  let logMsgStr = fromLogStr logMsg
      logMsgTxt = T.pack logMsgStr

  assertBool logMsgStr ("[2022-02-08 10:20:05][ThreadId " `T.isPrefixOf` logMsgTxt)
  assertBool logMsgStr ("][Warn] msg" `T.isSuffixOf` logMsgTxt)
#endif
  where
    fmt =
      MkLogFormatter
        { locStrategy = LocNone,
          newline = False,
          threadLabel = True,
          timezone = False
        }

    runLoggerThreadLabel :: Logger a -> Namespace -> IO a
    runLoggerThreadLabel (MkLogger rdr) ns =
      runReaderT rdr (MkEnv ns (Just "some-thread"))

{- ORMOLU_ENABLE -}

logLevelTH :: TestTree
logLevelTH = testCase "TH logging compiles" $ do
  runNoLoggingT $ do
    $(logTrace) "trace"
    $(logInfo) "info"
    $(logDebug) "debug"
    $(logWarn) "warn"
    $(logError) "error"
    $(logOther "custom") "other"
    $(logFatal) "fatal"

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

fromLogStr :: LogStr -> String
fromLogStr = Char8.unpack . logStrToBs

emptyNamespace :: Namespace
emptyNamespace = MkNamespace Seq.empty

shouldLevelTests :: TestTree
shouldLevelTests =
  testGroup
    "shouldLevel"
    [ traceProps,
      debugProps,
      infoProps,
      warnProps,
      errorProps,
      fatalProps,
      customProps,
      stdLevelsFollowOrd,
      otherSpecs
    ]

traceProps :: TestTree
traceProps = testPropertyNamed "Trace properties" "traceProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    -- trace logs everything
    assert $ LevelOther "Trace" `shouldLog` lvl

    if lvl `shouldLog` LevelOther "Trace"
      then assert $ isTrace lvl
      else
        assert $
          LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl

debugProps :: TestTree
debugProps = testPropertyNamed "Debug properties" "debugProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelDebug `shouldLog` lvl
      then
        assert $
          LevelDebug
            == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl

    if lvl `shouldLog` LevelDebug
      then
        assert $
          isTrace lvl
            || LevelDebug == lvl
      else
        assert $
          LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl

infoProps :: TestTree
infoProps = testPropertyNamed "Info properties" "infoProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelInfo `shouldLog` lvl
      then
        assert $
          LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || LevelDebug == lvl

    if lvl `shouldLog` LevelInfo
      then
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
      else
        assert $
          LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl

warnProps :: TestTree
warnProps = testPropertyNamed "Warn properties" "warnProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelWarn `shouldLog` lvl
      then
        assert $
          LevelWarn == lvl
            || LevelError == lvl
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl

    if lvl `shouldLog` LevelWarn
      then
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
      else
        assert $
          LevelError == lvl
            || isOtherNonTrace lvl

errorProps :: TestTree
errorProps = testPropertyNamed "Error properties" "errorProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    if LevelError `shouldLog` lvl
      then
        assert $
          LevelError == lvl
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl

    if lvl `shouldLog` LevelError
      then
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
      else assert $ isOtherNonTrace lvl

fatalProps :: TestTree
fatalProps = testPropertyNamed "Fatal properties" "fatalProps" $ do
  property $ do
    lvl <- forAll genAnyLevel

    -- Fatal logs nothing except itself and non-trace custom
    if LevelOther "Fatal" `shouldLog` lvl
      then
        assert $
          isFatal lvl
            || isOtherCustom lvl
      else do
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl

    if lvl `shouldLog` LevelOther "Fatal"
      then
        assert $
          isTrace lvl
            || LevelDebug == lvl
            || LevelInfo == lvl
            || LevelWarn == lvl
            || LevelError == lvl
            || isFatal lvl
      else
        assert $
          isOtherCustom lvl

customProps :: TestTree
customProps = testPropertyNamed "Custom properties" "customProps" $ do
  property $ do
    lvl <- forAll genAnyLevel
    customLvl <- forAll genText

    if LevelOther customLvl `shouldLog` lvl
      then case preview _LevelCustom lvl of
        Nothing -> do
          annotate "LevelOther <custom> `shouldLog` l should imply l is also LevelOther"
          failure
        Just l -> assert $ customLvl <= l
      else case preview _LevelCustom lvl of
        Nothing -> pure ()
        Just l -> assert $ customLvl > l

    if lvl `shouldLog` LevelOther customLvl
      then case preview _LevelCustom lvl of
        Nothing -> pure ()
        Just l -> assert $ l <= customLvl
      else case preview _LevelCustom lvl of
        Nothing -> do
          annotate "l `shouldLog` LevelOther <custom> should imply l is also LevelOther"
          failure
        Just l -> assert $ l > customLvl

stdLevelsFollowOrd :: TestTree
stdLevelsFollowOrd = testPropertyNamed "Std levels follow Ord" "stdLevelsFollowOrd" $ do
  property $ do
    lvl1 <- forAll genStdLevel
    lvl2 <- forAll genStdLevel

    if
      | lvl1 == lvl2 -> do
          assert $ lvl1 `shouldLog` lvl2
          assert $ lvl2 `shouldLog` lvl1
      | lvl1 < lvl2 -> assert $ lvl1 `shouldLog` lvl2
      | otherwise -> assert $ lvl2 `shouldLog` lvl1

otherSpecs :: TestTree
otherSpecs = testCase "LevelOther specs" $ do
  assertBool "Debug should not log trace" $ not $ shouldLog LevelDebug (LevelOther "Trace")
  assertBool "Debug should log custom" $ shouldLog LevelDebug (LevelOther "Custom")
  assertBool "Debug should log fatal" $ shouldLog LevelDebug (LevelOther "Fatal")

genStdLevel :: Gen LogLevel
genStdLevel =
  Gen.element
    [ LevelInfo,
      LevelDebug,
      LevelWarn,
      LevelError
    ]

genAnyLevel :: Gen LogLevel
genAnyLevel =
  Gen.choice
    [ pure (LevelOther "Trace"),
      pure LevelInfo,
      pure LevelDebug,
      pure LevelWarn,
      pure LevelError,
      pure (LevelOther "Fatal"),
      LevelOther <$> genText
    ]

genText :: Gen Text
genText = Gen.text (Range.linearFrom 0 0 20) Gen.unicode

isTrace :: LogLevel -> Bool
isTrace = is _LevelTrace

isFatal :: LogLevel -> Bool
isFatal = is _LevelFatal

isOtherCustom :: LogLevel -> Bool
isOtherCustom l =
  is _LevelOther l
    && isNot _LevelTrace l
    && isNot _LevelFatal l

isOtherNonTrace :: LogLevel -> Bool
isOtherNonTrace l = is _LevelOther l && isNot _LevelTrace l

isNot :: (Is k An_AffineFold) => Optic' k is s a -> s -> Bool
isNot o = not . is o

-- This is not lawful but it is useful here, so w/e
_LevelCustom :: Prism' LogLevel Text
_LevelCustom =
  prism
    LevelOther
    ( \case
        x@(LevelOther "Trace") -> Left x
        x@(LevelOther "Fatal") -> Left x
        LevelOther c -> Right c
        other -> Left other
    )
