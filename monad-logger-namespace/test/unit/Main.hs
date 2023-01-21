{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Control.Monad.Logger
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.LoggerNamespace
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (..),
    ZonedTime (ZonedTime),
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

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

instance MonadLoggerNamespace Logger where
  getNamespace = MkLogger id
  localNamespace f (MkLogger g) = MkLogger (g . f)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit Tests"
      [ formatBasic,
        formatNewline,
        formatTimezone,
        formatLocStable,
        formatLocPartial
      ]

formatBasic :: TestTree
formatBasic =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      BSL.fromStrict $
        logStrToBs logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = False
        }
    desc = "Formats a basic namespaced log"
    gpath = goldenPath </> "format-basic.golden"

formatNewline :: TestTree
formatNewline =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      BSL.fromStrict $
        logStrToBs logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = True,
          locStrategy = LocNone,
          timezone = False
        }
    desc = "Formats a log with a newline"
    gpath = goldenPath </> "format-newline.golden"

formatTimezone :: TestTree
formatTimezone =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      BSL.fromStrict $
        logStrToBs logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocNone,
          timezone = True
        }
    desc = "Formats a log with a timezone"
    gpath = goldenPath </> "format-timezone.golden"

formatLocStable :: TestTree
formatLocStable =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      BSL.fromStrict $
        logStrToBs logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocStable loc,
          timezone = False
        }
    desc = "Formats a log with stable loc"
    gpath = goldenPath </> "format-locstable.golden"

formatLocPartial :: TestTree
formatLocPartial =
  goldenVsStringDiff desc gdiff gpath $
    pure $
      BSL.fromStrict $
        logStrToBs logMsg
  where
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocPartial loc,
          timezone = False
        }
    desc = "Formats a log with partial loc"
    gpath = goldenPath </> "format-locpartial.golden"

formatNamespaced ::
  ( MonadLoggerNamespace m,
    MonadTime m
  ) =>
  LogFormatter ->
  m LogStr
formatNamespaced fmt =
  addNamespace "one" $
    addNamespace "two" $
      formatLog @_ @Text fmt LevelWarn "msg"

goldenPath :: FilePath
goldenPath = "test/unit/"

gdiff :: FilePath -> FilePath -> [FilePath]
gdiff ref new = ["diff", "-u", ref, new]

loc :: Loc
loc = Loc "filename" "pkg" "module" (1, 2) (3, 4)

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc
