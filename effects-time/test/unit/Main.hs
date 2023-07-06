{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, displayException, try)
import Control.Monad (void, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Fixed (Fixed (MkFixed))
import Data.List qualified as L
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.Time
  ( LocalTime (LocalTime),
    TimeSpec (MkTimeSpec),
    ZonedTime (ZonedTime),
  )
import Effects.Time qualified as MonadTime
import Hedgehog (Gen, annotate, annotateShow, diff, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as R
import Numeric.Natural (Natural)
import Optics.Core (view)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Read qualified as TR

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ classTests,
        timeSpecTests,
        localTimeTests,
        zonedTimeTests
      ]

classTests :: TestTree
classTests =
  testGroup
    "Class"
    [ getsSystemTime,
      getsSystemZonedTime,
      getsMonotonicTime
    ]

getsSystemTime :: TestTree
getsSystemTime =
  testCase "Retrieves system local time" $
    void MonadTime.getSystemTime

getsSystemZonedTime :: TestTree
getsSystemZonedTime =
  testCase "Retrieves system zoned time" $
    void MonadTime.getSystemZonedTime

getsMonotonicTime :: TestTree
getsMonotonicTime = testPropertyNamed desc "getsMonotonicTime" $
  property $ do
    t1 <- liftIO MonadTime.getMonotonicTime
    t2 <- liftIO MonadTime.getMonotonicTime
    annotateShow t1
    annotateShow t2
    diff t1 (<=) t2
  where
    desc = "getMonotonicTime is monotonic"

timeSpecTests :: TestTree
timeSpecTests =
  testGroup
    "TimeSpec"
    [ eqEquivClass,
      createFromSeconds,
      elimToSeconds,
      toFromSecondsEpsilon,
      fromToSecondsEpsilon,
      createFromNanoSeconds,
      elimToNanoseconds,
      toFromNatRoundTrip,
      fromToNatRoundTrip,
      diffsTimeSpec,
      diffTimeSpecCommutes,
      normalizesTimeSpec,
      normalizeInvariant,
      timesAction
    ]

eqEquivClass :: TestTree
eqEquivClass = testPropertyNamed desc "eqEquivClass" $
  property $ do
    ts@(MkTimeSpec s ns) <- forAll genTimeSpec
    let ts' = MkTimeSpec 0 (s * 1_000_000_000 + ns)
    ts === ts'
  where
    desc = "Eq equivalence class"

createFromSeconds :: TestTree
createFromSeconds =
  testCase "Creates TimeSpec from Double seconds" $
    expected @=? MonadTime.fromSeconds 10.123456789
  where
    expected = MkTimeSpec 10 123456789

createFromNanoSeconds :: TestTree
createFromNanoSeconds =
  testCase "Creates TimeSpec from Natural nanoseconds" $
    expected @=? MonadTime.fromNanoSeconds 10_123_456_789
  where
    expected = MkTimeSpec 10 123456789

elimToSeconds :: TestTree
elimToSeconds =
  testCase "Maps TimeSpec to seconds" $
    10.123456789 @=? MonadTime.toSeconds (MkTimeSpec 10 123_456_789)

toFromSecondsEpsilon :: TestTree
toFromSecondsEpsilon = testPropertyNamed desc "toFromSecondsEpsilon" $
  property $ do
    s <- forAll genDouble
    let ts = MonadTime.fromSeconds s
        s' = MonadTime.toSeconds ts
    annotateShow ts

    diff (abs (s - s')) (<) 1
  where
    desc = "(toSeconds . fromSeconds) x ~= x (up to 1 sec)"

fromToSecondsEpsilon :: TestTree
fromToSecondsEpsilon = testPropertyNamed desc "fromToSecondsEpsilon" $
  property $ do
    ts <- forAll genTimeSpec
    let s = MonadTime.toSeconds ts
        ts' = MonadTime.fromSeconds s
    annotateShow s

    toSeconds ts === toSeconds ts'
  where
    toSeconds = view #sec . MonadTime.normalizeTimeSpec
    desc = "(toSeconds . fromSeconds) x ~= x (up to 1 sec)"

toFromNatRoundTrip :: TestTree
toFromNatRoundTrip = testPropertyNamed desc "toFromNatRoundTrip" $
  property $ do
    ns <- forAll genNanoSeconds
    let ts = MonadTime.fromNanoSeconds ns
        ns' = MonadTime.toNanoSeconds ts
    annotateShow ts
    ns === ns'
  where
    desc = "toNanoSeconds . fromNanoSeconds == id"

fromToNatRoundTrip :: TestTree
fromToNatRoundTrip = testPropertyNamed desc "fromToNatRoundTrip" $
  property $ do
    ts <- forAll genTimeSpec
    let ns = MonadTime.toNanoSeconds ts
        ts' = MonadTime.fromNanoSeconds ns
    annotateShow ns
    ts === ts'
  where
    desc = "fromNanoSeconds . toNanoSeconds == id"

elimToNanoseconds :: TestTree
elimToNanoseconds =
  testCase "Maps TimeSpec to nanoseconds" $
    10123456789 @=? MonadTime.toNanoSeconds (MkTimeSpec 10 123_456_789)

diffsTimeSpec :: TestTree
diffsTimeSpec = testCase "Diffs TimeSpecs" $ do
  MkTimeSpec 10 864197532 @=? MonadTime.diffTimeSpec t1 t2
  MkTimeSpec 10 864197532 @=? MonadTime.diffTimeSpec t2 t1
  where
    t1 = MkTimeSpec 10 123_456_789
    t2 = MkTimeSpec 20 987_654_321

diffTimeSpecCommutes :: TestTree
diffTimeSpecCommutes = testPropertyNamed desc "diffsTimeSpec2" $
  property $ do
    ts1 <- forAll genTimeSpec
    ts2 <- forAll genTimeSpec
    let d1 = MonadTime.diffTimeSpec ts1 ts2
        d2 = MonadTime.diffTimeSpec ts2 ts1
    d1 === d2
  where
    desc = "diffTimeSpec is commutative"

normalizesTimeSpec :: TestTree
normalizesTimeSpec =
  testCase "Normalizes TimeSpec" $
    MkTimeSpec 55 123456789 @=? MonadTime.normalizeTimeSpec t
  where
    t = MkTimeSpec 10 45_123_456_789

normalizeInvariant :: TestTree
normalizeInvariant = testPropertyNamed desc "normalizeInvariant" $
  property $ do
    ts <- forAll genTimeSpec
    let ts'@(MkTimeSpec _ ns') = MonadTime.normalizeTimeSpec ts

    annotateShow ts'

    -- nanoseconds < 1 second
    diff ns' (<) 1_000_000_000

    -- equivalence class
    ts === ts'
  where
    desc = "Normalizes TimeSpec"

timesAction :: TestTree
timesAction = testCase "Times an action" $ do
  ts <- MonadTime.withTiming_ (threadDelay 1_000_000)
  -- NOTE: Evidently this is pretty imprecise on a mac, so we make the
  -- threshold up to +/- 0.9s.
  assertBool (show ts <> " >= 0.1 s") $ ts >= MkTimeSpec 0 100_000_000
  assertBool (show ts <> " <= 1.9 s") $ ts <= MkTimeSpec 1 900_000_000

localTimeTests :: TestTree
localTimeTests =
  testGroup
    "LocalTime"
    [ formatsLocalTime,
      parsesLocalTime,
      formatParseLocalTimeRoundTrip,
      parseFormatLocalTimeEpsilon,
      parsesLocalTimeCallStack
    ]

zonedTimeTests :: TestTree
zonedTimeTests =
  testGroup
    "ZonedTime"
    [ formatsZonedTime,
      parsesZonedTime,
      formatParseZonedTimeRoundTrip,
      parseFormatZonedTimeEpsilon,
      parsesZonedTimeCallStack
    ]

formatsLocalTime :: TestTree
formatsLocalTime =
  testCase "Formats LocalTime" $
    "2022-02-08 10:20:05" @=? MonadTime.formatLocalTime localTime

parsesLocalTime :: TestTree
parsesLocalTime = testCase "Parses LocalTime" $ do
  lt <- MonadTime.parseLocalTime "2022-02-08 10:20:05"
  localTime @=? lt

formatParseLocalTimeRoundTrip :: TestTree
formatParseLocalTimeRoundTrip = testPropertyNamed desc "formatParseLocalTimeRoundTrip" $
  property $ do
    str <- forAll genLocalTimeString
    lt <- MonadTime.parseLocalTime str
    annotateShow lt

    let str' = MonadTime.formatLocalTime lt

    str === str'
  where
    desc = "formatLocalTime . parseLocalTime == id"

parseFormatLocalTimeEpsilon :: TestTree
parseFormatLocalTimeEpsilon = testPropertyNamed desc "parseFormatLocalTimeEpsilon" $
  property $ do
    lt <- forAll genLocalTime
    let str = MonadTime.formatLocalTime lt
    annotate str

    lt' <- MonadTime.parseLocalTime str

    diff lt eqLocalTimeEpsilon lt'
  where
    desc = "(parseLocalTime . formatLocalTime) x ~= x (up to < 1 second)"

parsesLocalTimeCallStack :: TestTree
parsesLocalTimeCallStack = testCase "Parses LocalTime failure gives CallStack" $ do
  try @SomeException parseAction >>= \case
    Left e -> assertResults expected (L.lines $ stableCallStack e)
    Right _ -> assertFailure "Error: did not catch expected exception."
  where
    parseAction = MonadTime.parseLocalTimeCallStack "2022-02-08 10:20:05 UTC"
#if WINDOWS && GHC_9_4
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0 UTC\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src\\Effects\\Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseLocalTimeCallStack, called at test\\unit\\Main.hs:0:0 in main:Main"
      ]
#elif WINDOWS
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0 UTC\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src\\\\Effects\\\\Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseLocalTimeCallStack, called at test\\\\unit\\\\Main.hs:0:0 in main:Main"
      ]
#else
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0 UTC\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src/Effects/Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseLocalTimeCallStack, called at test/unit/Main.hs:0:0 in main:Main"
      ]
#endif

formatsZonedTime :: TestTree
formatsZonedTime =
  testCase "Formats ZonedTime" $
    "2022-02-08 10:20:05 UTC" @=? MonadTime.formatZonedTime zonedTime

parsesZonedTime :: TestTree
parsesZonedTime = testCase "Parses ZonedTime" $ do
  ZonedTime lt tz <- MonadTime.parseZonedTime "2022-02-08 10:20:05 UTC"
  let ZonedTime expectedLt expectedTz = zonedTime
  expectedLt @=? lt
  expectedTz @=? tz

formatParseZonedTimeRoundTrip :: TestTree
formatParseZonedTimeRoundTrip = testPropertyNamed desc "formatParseZonedTimeRoundTrip" $
  property $ do
    str <- forAll genZonedTimeString
    lt <- MonadTime.parseZonedTime str
    annotateShow lt

    let str' = MonadTime.formatZonedTime lt

    str === str'
  where
    desc = "formatZonedTime . parseZonedTime == id"

parseFormatZonedTimeEpsilon :: TestTree
parseFormatZonedTimeEpsilon = testPropertyNamed desc "parseFormatZonedTimeEpsilon" $
  property $ do
    zt <- forAll genZonedTime
    let str = MonadTime.formatZonedTime zt
    annotate str

    zt' <- MonadTime.parseZonedTime str

    diff zt eqZonedTimeEpsilon zt'
  where
    desc = "(parseZonedTime . formatZonedTime) x ~= x (up to < 1 second)"

parsesZonedTimeCallStack :: TestTree
parsesZonedTimeCallStack =
  testCase "Parses ZonedTime failure gives CallStack" $
    try @SomeException parseAction >>= \case
      Left e -> assertResults expected (L.lines $ stableCallStack e)
      Right _ -> assertFailure "Error: did not catch expected exception."
  where
    parseAction = MonadTime.parseZonedTimeCallStack "2022-02-08 10:20:05"
#if WINDOWS && GHC_9_4
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src\\Effects\\Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseZonedTimeCallStack, called at test\\unit\\Main.hs:0:0 in main:Main"
      ]
#elif WINDOWS
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src\\\\Effects\\\\Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseZonedTimeCallStack, called at test\\\\unit\\\\Main.hs:0:0 in main:Main"
      ]
#else
    expected =
      [ "user error (parseTimeM: no parse of \"0-0-0 0:0:0\")",
        "CallStack (from HasCallStack):",
        "  addCS, called at src/Effects/Time.hs:0:0 in effects-time-0.0-<pkg>:Effects.Time",
        "  parseZonedTimeCallStack, called at test/unit/Main.hs:0:0 in main:Main"
      ]
#endif

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

eqLocalTimeEpsilon :: LocalTime -> LocalTime -> Bool
eqLocalTimeEpsilon
  (LocalTime d1 (TimeOfDay h1 m1 (MkFixed s1)))
  (LocalTime d2 (TimeOfDay h2 m2 (MkFixed s2))) =
    d1 == d2
      && h1 == h2
      && m1 == m2
      -- Second difference must be less than 1
      -- (== < 1_000_000_000_000 picoseconds)
      && abs (s1 - s2) < 1_000_000_000_000

eqZonedTimeEpsilon :: ZonedTime -> ZonedTime -> Bool
eqZonedTimeEpsilon (ZonedTime t1 z1) (ZonedTime t2 z2) =
  eqLocalTimeEpsilon t1 t2
    && z1 == z2

genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay
  where
    genDay = fromOrdinalDate <$> genYear <*> genDayOfYear
    genYear = Gen.integral (R.linear 0 2022)
    genDayOfYear = Gen.integral (R.linear 0 364)

    genTimeOfDay = TimeOfDay <$> genHour <*> genMin <*> genSec
    genHour = Gen.integral (R.linear 0 23)
    genMin = Gen.integral (R.linear 0 59)
    -- Fixed precision pico seconds 1e-12
    genSec = MkFixed <$> Gen.integral (R.linear 0 59_000_000_000_000)

genZonedTime :: Gen ZonedTime
genZonedTime = ZonedTime <$> genLocalTime <*> genTz
  where
    genTz = pure utc

genLocalTimeString :: Gen String
genLocalTimeString =
  toDate
    <$> genYear
    <*> genMonth
    <*> genDay
    <*> genHour
    <*> genMin
    <*> genSec
  where
    toDate y m d h mn s =
      mconcat
        [ y,
          "-",
          m,
          "-",
          d,
          " ",
          h,
          ":",
          mn,
          ":",
          s
        ]
    genYear = Gen.list (R.singleton 4) Gen.digit
    genMonth = Gen.element $ fmap (pad2 . show @Int) [1 .. 12]
    genDay = Gen.element $ fmap (pad2 . show @Int) [1 .. 28]
    genHour = Gen.element $ fmap (pad2 . show @Int) [1 .. 23]
    genMin = Gen.element $ fmap (pad2 . show @Int) [0 .. 59]
    genSec = genMin
    pad2 s
      | length s == 1 = '0' : s
      | otherwise = s

genZonedTimeString :: Gen String
genZonedTimeString =
  (<>)
    <$> genLocalTimeString
    <*> Gen.element
      [ " UTC",
        " UT",
        " GMT",
        " EST",
        " EDT",
        " CST",
        " CDT",
        " MST",
        " MDT",
        " PST",
        " PDT",
        " +1300"
      ]

genNanoSeconds :: Gen Natural
genNanoSeconds = Gen.integral (R.linearFrom 1_000_000_000 0 100_000_000_000)

genDouble :: Gen Double
genDouble = Gen.double (R.linearFracFrom 1 0 100)

genTimeSpec :: Gen TimeSpec
genTimeSpec = MkTimeSpec <$> genSec <*> genNSec
  where
    genSec = Gen.integral (R.linearFrom 5 0 10)
    genNSec = Gen.integral (R.linearFrom 0 0 10_000_000_000)

stableCallStack :: (Exception e) => e -> String
stableCallStack = stripPkgName . zeroNums . displayException

-- A bit overzealous since we don't need all numbers zeroed, but it's not a
-- big deal
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

-- crude, but it works
stripPkgName :: String -> String
stripPkgName [] = []
stripPkgName (L.stripPrefix "effects-time-0.0-" -> Just rest) =
  "effects-time-0.0-<pkg>" ++ skipUntilColon rest
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
          ")."
        ]
  zipWithM_ (@=?) expected results
  where
    lenExpected = length expected
    lenResults = length results
