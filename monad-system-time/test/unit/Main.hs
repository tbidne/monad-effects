module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Fixed (Fixed (MkFixed))
import Data.Functor ((<&>))
import Data.String (IsString (fromString))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.Exception (displayCallStack)
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
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

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
  testCase "Retrieves system zoned timeee" $
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
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        show $
          MonadTime.fromSeconds 10.123456789
  where
    desc = "Creates TimeSpec from Double seconds"
    gpath = goldenPath </> "timespec-create-seconds.golden"

createFromNanoSeconds :: TestTree
createFromNanoSeconds =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        show $
          MonadTime.fromNanoSeconds 10_123_456_789
  where
    desc = "Creates TimeSpec from Natural nanoseconds"
    gpath = goldenPath </> "timespec-create-nanoseconds.golden"

elimToSeconds :: TestTree
elimToSeconds =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        show $
          MonadTime.toSeconds $
            MkTimeSpec 10 123_456_789
  where
    desc = "Maps TimeSpec to seconds"
    gpath = goldenPath </> "timespec-elim-seconds.golden"

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
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        show $
          MonadTime.toNanoSeconds $
            MkTimeSpec 10 123_456_789
  where
    desc = "Maps TimeSpec to nanoseconds"
    gpath = goldenPath </> "timespec-elim-nanoseconds.golden"

diffsTimeSpec :: TestTree
diffsTimeSpec =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        unlines
          [ show $ MonadTime.diffTimeSpec t1 t2,
            show $ MonadTime.diffTimeSpec t2 t1,
            show $ MonadTime.diffTimeSpec t1 t2 == MonadTime.diffTimeSpec t2 t1
          ]
  where
    t1 = MkTimeSpec 10 123_456_789
    t2 = MkTimeSpec 20 987_654_321
    desc = "Diffs TimeSpecs"
    gpath = goldenPath </> "timespec-diff.golden"

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
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        show $
          MonadTime.normalizeTimeSpec t
  where
    t = MkTimeSpec 10 45_123_456_789
    desc = "Normalizes TimeSpec"
    gpath = goldenPath </> "timespec-normalize.golden"

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
  assertBool (show ts <> " >= 0.9 s") $ ts >= MkTimeSpec 0 900_000_000
  assertBool (show ts <> " <= 1.1 s") $ ts <= MkTimeSpec 1 100_000_000

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
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        MonadTime.formatLocalTime localTime
  where
    desc = "Formats LocalTime"
    gpath = goldenPath </> "localtime-format.golden"

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
parsesLocalTimeCallStack =
  goldenVsStringDiff desc gdiff gpath $
    try @SomeException parseAction <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    parseAction = MonadTime.parseLocalTimeCallStack "2022-02-08 10:20:05 UTC"
    desc = "Parses LocalTime failure gives CallStack"
    gpath = goldenPath </> "localtime-parse-callstack.golden"

formatsZonedTime :: TestTree
formatsZonedTime =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      fromString $
        MonadTime.formatZonedTime zonedTime
  where
    desc = "Formats ZonedTime"
    gpath = goldenPath </> "zonedtime-format.golden"

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
  goldenVsStringDiff desc gdiff gpath $
    try @SomeException parseAction <&> \case
      Left e -> fromString $ stableCallStack e
      Right _ -> "Error: did not catch expected exception."
  where
    parseAction = MonadTime.parseZonedTimeCallStack "2022-02-08 10:20:05"
    desc = "Parses ZonedTime failure gives CallStack"
    gpath = goldenPath </> "zonedtime-parse-callstack.golden"

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

goldenPath :: FilePath
goldenPath = "test/unit/"

gdiff :: FilePath -> FilePath -> [FilePath]
gdiff ref new = ["diff", "-u", ref, new]

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

stableCallStack :: Exception e => e -> String
stableCallStack = unlines . take 2 . lines . displayCallStack
