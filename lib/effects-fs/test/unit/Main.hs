{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removeDirectoryRecursiveIfExists,
    removePathForcibly,
  )
import Effects.FileSystem.Utils (OsPath, osp, (</>))
import Effects.FileSystem.Utils qualified as Utils
import Misc qualified
import PathReader qualified
import PathWriter qualified
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Misc.tests args,
          PathReader.tests args,
          PathWriter.tests args
        ]

setup :: IO OsPath
setup = do
  tmpDir <-
    (\s -> s </> [osp|effects-fs|] </> [osp|unit|])
      <$> getTemporaryDirectory
  removeDirectoryRecursiveIfExists tmpDir
  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> Utils.decodeOsToFpShow fp
