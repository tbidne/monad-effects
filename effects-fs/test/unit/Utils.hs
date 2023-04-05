{-# LANGUAGE CPP #-}

module Utils
  ( strToPath,
    pathToStr,
  )
where

import Effects.FileSystem.Path (Path)
#if USE_OS_PATH
import System.OsPath qualified as FP

strToPath :: String -> Path
strToPath = FP.pack . fmap FP.unsafeFromChar

pathToStr :: Path -> String
pathToStr = fmap FP.toChar . FP.unpack
#else

strToPath :: String -> Path
strToPath = id

pathToStr :: Path -> String
pathToStr = id
#endif
