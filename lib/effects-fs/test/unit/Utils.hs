module Utils
  ( strToPath,
    pathToStr,
  )
where

import System.OsPath (OsPath)
import System.OsPath qualified as FP

strToPath :: String -> OsPath
strToPath = FP.pack . fmap FP.unsafeFromChar

pathToStr :: OsPath -> String
pathToStr = fmap FP.toChar . FP.unpack
