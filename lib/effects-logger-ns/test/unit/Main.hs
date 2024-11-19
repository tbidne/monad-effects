{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main (main) where

import Control.Concurrent qualified as CC
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (Loc (Loc), NoLoggingT (runNoLoggingT))
import Control.Monad.Logger.CallStack (LogLevel (LevelDebug))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.ByteString.Char8 qualified as Char8
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Concurrent.Thread qualified as Thread
import Effects.LoggerNS
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
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
import Effects.LoggerNS qualified as LoggerNS
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
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as Range
import Optics.Core (An_AffineFold, Is, Optic', Prism', preview, prism, set', (^.))
import Optics.Core.Extras (is)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

data Env = MkEnv
  { namespace :: Namespace,
    threadLabel :: Maybe String
  }

newtype Logger a = MkLogger {unLogger :: (ReaderT Env IO a)}
  deriving (Applicative, Functor, Monad) via (ReaderT Env IO)

runLogger :: Logger a -> Namespace -> IO a
runLogger (MkLogger rdr) ns = runReaderT rdr (MkEnv ns Nothing)

instance MonadThread Logger where
  myThreadId = MkLogger $ liftIO CC.myThreadId

#if MIN_VERSION_base(4, 18, 0)
  threadLabel _ = MkLogger $ asks (.threadLabel)
#endif

instance MonadTime Logger where
  getSystemZonedTime = pure zonedTime
  getMonotonicTime = pure 50

instance MonadLogger Logger where
  monadLoggerLog _loc _src _lvl _msg = pure ()

instance MonadLoggerNS Logger where
  getNamespace = MkLogger $ asks (\(MkEnv ns _) -> ns)
  localNamespace f =
    MkLogger
      . local (\env -> MkEnv (f env.namespace) env.threadLabel)
      . unLogger

main :: IO ()
main =
  defaultMain $
    testGroup
      "Effects.LoggerNS"
      [ formatTests,
        logLevelTH,
        shouldLevelTests,
        accessorTests
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  "[2022-02-08 10:20:05][one.two][Warn] msg" @=? fromLogStr logMsg
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
  "[2022-02-08 10:20:05][one.two][Trace] msg" @=? fromLogStr logMsg
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
  "[2022-02-08 10:20:05][one.two][Fatal] msg" @=? fromLogStr logMsg
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  "[2022-02-08 10:20:05][one.two][Warn] msg\n" @=? fromLogStr logMsg
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  "[2022-02-08 10:20:05 UTC][one.two][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  "[2022-02-08 10:20:05][one.two][filename][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  "[2022-02-08 10:20:05][one.two][filename:1:2][Warn] msg" @=? fromLogStr logMsg
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
  logMsg <- runLogger (formatNamespaced fmt) emptyNamespace
  let logMsgStr = fromLogStr logMsg
      logMsgTxt = T.pack logMsgStr

  -- The thread id itself is non-deterministic, so we have to split the
  -- asserts up.
  assertBool logMsgStr ("[2022-02-08 10:20:05][ThreadId " `T.isPrefixOf` logMsgTxt)
  assertBool logMsgStr ("][one.two][Warn] msg" `T.isSuffixOf` logMsgTxt)
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
  logMsg <- runLoggerThreadLabel (formatNamespaced fmt) emptyNamespace
#if MIN_VERSION_base(4, 18, 0)
  "[2022-02-08 10:20:05][some-thread][one.two][Warn] msg"
    @=? fromLogStr logMsg
#else

  -- TODO: Remove this case once we unconditionally have base >= 4.18
  let logMsgStr = fromLogStr logMsg
      logMsgTxt = T.pack logMsgStr

  assertBool logMsgStr ("[2022-02-08 10:20:05][ThreadId " `T.isPrefixOf` logMsgTxt)
  assertBool logMsgStr ("][one.two][Warn] msg" `T.isSuffixOf` logMsgTxt)
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

formatNamespaced ::
  ( MonadLoggerNS m,
    MonadThread m,
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
    MonadThread m,
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
    MonadThread m,
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

accessorTests :: TestTree
accessorTests =
  testGroup
    "Accessors"
    [ testLogFormatterAccessors
    ]

testLogFormatterAccessors :: TestTree
testLogFormatterAccessors = testPropertyNamed desc "testLogFormatterAccessors" $ do
  property $ do
    logFormatter <- forAll genLogFormatter
    locStrategy' <- forAll genLocStrategy
    bool' <- forAll HG.bool

    -- get
    logFormatter.locStrategy === logFormatter ^. #locStrategy
    logFormatter.newline === logFormatter ^. #newline
    logFormatter.threadLabel === logFormatter ^. #threadLabel
    logFormatter.timezone === logFormatter ^. #timezone

    -- set
    logFormatter {locStrategy = locStrategy'} === set' #locStrategy locStrategy' logFormatter
    logFormatter {newline = bool'} === set' #newline bool' logFormatter
    logFormatter {LoggerNS.threadLabel = bool'} === set' #threadLabel bool' logFormatter
    logFormatter {timezone = bool'} === set' #timezone bool' logFormatter
  where
    desc = "LogFormatter"

genLogFormatter :: Gen LogFormatter
genLogFormatter = do
  MkLogFormatter
    <$> genLocStrategy
    <*> HG.bool
    <*> HG.bool
    <*> HG.bool

genLocStrategy :: Gen LocStrategy
genLocStrategy =
  HG.choice
    [ pure LocNone,
      LocStable <$> genLoc,
      LocPartial <$> genLoc
    ]

genLoc :: Gen Loc
genLoc = do
  Loc
    <$> genStr
    <*> genStr
    <*> genStr
    <*> genCharPos
    <*> genCharPos
  where
    genStr = HG.list (Range.linear 0 100) HG.unicode
    genCharPos = (,) <$> genInt <*> genInt
    genInt = HG.integral (Range.linear 0 1_000)
