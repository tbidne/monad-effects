{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (when)
import Effects.FileSystem.OsPath (OsPath, osp, (</>))
import Posix qualified
import System.Directory.OsPath qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Posix.tests args
        ]

setup :: IO OsPath
setup = do
  tmpDir <-
    (\s -> s </> [osp|effects-unix|] </> [osp|unit|])
      <$> Dir.getTemporaryDirectory

  exists <- Dir.doesDirectoryExist tmpDir
  when exists (Dir.removeDirectoryRecursive tmpDir)
  Dir.createDirectoryIfMissing True tmpDir

  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> show fp
