{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Unit.Effects.Logger.Namespace (tests) where

import Control.Monad (void)
import Control.Monad.Logger (Loc (Loc))
import Control.Monad.Logger.CallStack (LogLevel)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 qualified as Char8
import Data.Functor.Identity (Identity (Identity))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Effects.Concurrent.Thread (MonadThread)
import Effects.Logger
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
    LogLevel (LevelInfo, LevelOther),
    LogStr,
    MonadLogger,
  )
import Effects.Logger qualified as Logger
import Effects.Logger.Namespace
  ( HasNamespace,
    MonadLoggerNS,
    Namespace (MkNamespace),
    addNamespace,
    logStrToBs,
  )
import Effects.Logger.Namespace qualified as NS
import Effects.Time (MonadTime)
import Hedgehog
  ( Gen,
    forAll,
    property,
    (===),
  )
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as Range
import Optics.Core
  ( A_Getter,
    A_Lens,
    A_Setter,
    An_Iso,
    Is,
    LabelOptic (labelOptic),
    LabelOptic',
    Prism',
    iso,
    lensVL,
    prism,
    set',
    (^.),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Effects.Logger.Namespace"
    [ formatTests,
      miscTests
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
  logMsg <- runLogger (formatTraceNamespaced fmt) emptyNamespace
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
  logMsg <- runLogger (formatFatalNamespaced fmt) emptyNamespace
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

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

fromLogStr :: LogStr -> String
fromLogStr = Char8.unpack . logStrToBs

emptyNamespace :: Namespace
emptyNamespace = MkNamespace Seq.empty

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

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testLogFormatterAccessors,
      testOpticsInference
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
    logFormatter {Logger.threadLabel = bool'} === set' #threadLabel bool' logFormatter
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

testOpticsInference :: TestTree
testOpticsInference = testCase "Optics type inference is reliable" $ do
  () @=? runEnv (addNamespace "ns" $ pure ()) lensEnv
  () @=? runEnv usesNamespace1 lensEnv
  () @=? runEnv usesNamespace2 lensEnv
  () @=? runEnv usesNamespace3 lensEnv

  () @=? runEnv (void $ NS.formatLog defFmt LevelInfo ("" :: Text)) lensEnv
  () @=? runEnv usesFormat1 lensEnv
  () @=? runEnv usesFormat2 lensEnv
  () @=? runEnv usesFormat3 lensEnv

  () @=? runEnv (addNamespace "ns" $ pure ()) isoEnv
  () @=? runEnv usesNamespace1 isoEnv
  () @=? runEnv usesNamespace2 isoEnv
  () @=? runEnv usesNamespace3 isoEnv

  () @=? runEnv (void $ NS.formatLog defFmt LevelInfo ("" :: Text)) isoEnv
  () @=? runEnv usesFormat1 isoEnv
  () @=? runEnv usesFormat2 isoEnv
  () @=? runEnv usesFormat3 isoEnv
  where
    runEnv :: ReaderT env M a -> env -> a
    runEnv m e = unM $ runReaderT m e

    lensEnv = MkLensEnv "lens"
    isoEnv = MkIsoEnv "iso"

-- | Exists for MonadLogger instance.
newtype M a = MkM {unM :: a}
  deriving stock (Functor)
  deriving (Applicative, Monad) via Identity

instance MonadThread M

instance MonadTime M

instance MonadLogger M where
  monadLoggerLog _ _ _ _ = pure ()

usesNamespace1 ::
  ( Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  ) =>
  m ()
usesNamespace1 = addNamespace "ns" $ pure ()

usesNamespace2 :: (HasNamespace m env k) => m ()
usesNamespace2 = addNamespace "ns" $ pure ()

usesNamespace3 :: (MonadLoggerNS m env k) => m ()
usesNamespace3 = addNamespace "ns" $ pure ()

usesFormat1 ::
  ( Is k A_Getter,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
usesFormat1 = void $ NS.formatLog defFmt LevelInfo ("" :: Text)

usesFormat2 ::
  ( HasNamespace m env k,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
usesFormat2 = void $ NS.formatLog defFmt LevelInfo ("" :: Text)

usesFormat3 ::
  ( MonadLoggerNS m env k,
    MonadThread m,
    MonadTime m
  ) =>
  m ()
usesFormat3 = void $ NS.formatLog defFmt LevelInfo ("" :: Text)

defFmt :: LogFormatter
defFmt = MkLogFormatter LocNone False False False

newtype LensEnv = MkLensEnv Namespace

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k LensEnv LensEnv x y
  where
  labelOptic =
    lensVL $ \f (MkLensEnv a1) ->
      fmap
        MkLensEnv
        (f a1)

newtype IsoEnv = MkIsoEnv Namespace

instance
  ( k ~ An_Iso,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k IsoEnv IsoEnv x y
  where
  labelOptic = iso (\(MkIsoEnv x) -> x) MkIsoEnv
