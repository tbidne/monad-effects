{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Logger (Loc (Loc), NoLoggingT (runNoLoggingT))
import Control.Monad.Logger.CallStack (LogLevel (LevelDebug))
import Data.ByteString.Char8 qualified as Char8
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.LoggerNS
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, timezone),
    LogLevel (LevelError, LevelInfo, LevelOther, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
    MonadLoggerNS (getNamespace, localNamespace),
    Namespace (MkNamespace),
    addNamespace,
    formatLog,
    logDebug,
    logError,
    logFatal,
    logInfo,
    logOther,
    logStrToBs,
    logTrace,
    logWarn,
    shouldLog,
    _LevelFatal,
    _LevelOther,
    _LevelTrace,
  )
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
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
import Optics.Core (An_AffineFold, Is, Optic', Prism', preview, prism)
import Optics.Core.Extras (is)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

newtype Logger a = MkLogger {runLogger :: Namespace -> a}
  deriving stock (Functor)

instance Applicative Logger where
  pure x = MkLogger $ const x
  MkLogger f <*> MkLogger g = MkLogger $ \ns -> f ns (g ns)

instance Monad Logger where
  MkLogger f >>= k = MkLogger $ \ns ->
    let a = f ns
        MkLogger mkB = k a
     in mkB ns

instance MonadTime Logger where
  getSystemZonedTime = pure zonedTime
  getMonotonicTime = pure 50

instance MonadLogger Logger where
  monadLoggerLog _loc _src _lvl _msg = pure ()

instance MonadLoggerNS Logger where
  getNamespace = MkLogger id
  localNamespace f (MkLogger g) = MkLogger (g . f)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effects.LoggerNS"
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
      formatLocPartial
    ]

formatBasic :: TestTree
formatBasic =
  testCase "Formats a basic namespaced log" $
    "[2022-02-08 10:20:05][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = False
        }

formatBasicTrace :: TestTree
formatBasicTrace =
  testCase "Formats a trace log" $
    "[2022-02-08 10:20:05][one.two][Trace] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatTrace fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = False
        }

formatBasicFatal :: TestTree
formatBasicFatal =
  testCase "Formats a fatal log" $
    "[2022-02-08 10:20:05][one.two][Fatal] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatFatal fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = False
        }

formatNewline :: TestTree
formatNewline =
  testCase "Formats a log with a newline" $
    "[2022-02-08 10:20:05][one.two][Warn] msg\n" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = True,
          locStrategy = LocNone,
          timezone = False
        }

formatTimezone :: TestTree
formatTimezone =
  testCase "Formats a log with a timezone" $
    "[2022-02-08 10:20:05 UTC][one.two][Warn] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = True
        }

formatLocStable :: TestTree
formatLocStable =
  testCase "Formats a log with stable loc" $
    "[2022-02-08 10:20:05][one.two][Warn][filename] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocStable loc,
          timezone = False
        }

formatLocPartial :: TestTree
formatLocPartial =
  testCase "Formats a log with partial loc" $
    "[2022-02-08 10:20:05][one.two][Warn][filename:1:2] msg" @=? fromLogStr logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) emptyNamespace
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocPartial loc,
          timezone = False
        }

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

formatNamespaced ::
  ( MonadLoggerNS m,
    MonadTime m
  ) =>
  LogFormatter ->
  m LogStr
formatNamespaced fmt =
  addNamespace "one" $
    addNamespace "two" $
      formatLog @_ @Text fmt LevelWarn "msg"

formatTrace ::
  ( MonadLoggerNS m,
    MonadTime m
  ) =>
  LogFormatter ->
  m LogStr
formatTrace fmt =
  addNamespace "one" $
    addNamespace "two" $
      formatLog @_ @Text fmt (LevelOther "Trace") "msg"

formatFatal ::
  ( MonadLoggerNS m,
    MonadTime m
  ) =>
  LogFormatter ->
  m LogStr
formatFatal fmt =
  addNamespace "one" $
    addNamespace "two" $
      formatLog @_ @Text fmt (LevelOther "Fatal") "msg"

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

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
    assert $ (LevelOther "Trace") `shouldLog` lvl

    if lvl `shouldLog` (LevelOther "Trace")
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
          lvl == LevelInfo
            || lvl == LevelWarn
            || lvl == LevelError
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || lvl == LevelDebug

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
          lvl == LevelWarn
            || lvl == LevelError
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || lvl == LevelDebug
            || lvl == LevelInfo

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
          lvl == LevelError
            || isOtherNonTrace lvl
      else
        assert $
          isTrace lvl
            || lvl == LevelDebug
            || lvl == LevelInfo
            || lvl == LevelWarn

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
    if (LevelOther "Fatal") `shouldLog` lvl
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

    if lvl `shouldLog` (LevelOther "Fatal")
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
stdLevelsFollowOrd = testPropertyNamed "Trace is the lowest level" "traceLowest" $ do
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
