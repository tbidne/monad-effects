{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Control.Monad.Logger (Loc (Loc), LogLevel (LevelWarn))
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay), utc)
import Effects.LoggerNS
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (..),
    LogStr,
    MonadLogger (..),
    MonadLoggerNS (..),
    Namespace,
    addNamespace,
    formatLog,
    logStrToBs,
  )
import Effects.Time
  ( LocalTime (LocalTime),
    MonadTime (..),
    ZonedTime (ZonedTime),
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

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
      "Unit Tests"
      [ formatBasic,
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
    logMsg = runLogger (formatNamespaced fmt) []
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
    logMsg = runLogger (formatNamespaced fmt) []
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
    logMsg = runLogger (formatNamespaced fmt) []
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
    logMsg = runLogger (formatNamespaced fmt) []
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
    logMsg = runLogger (formatNamespaced fmt) []
    fmt =
      MkLogFormatter
        { newline = False,
          locStrategy = LocPartial loc,
          timezone = False
        }

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
