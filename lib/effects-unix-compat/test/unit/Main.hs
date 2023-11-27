module Main (main) where

import Control.Monad (when)
import PosixCompat qualified
import System.Directory qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup, withResource)

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ PosixCompat.tests args
        ]

-- NOTE: FilePath and not OsPath as the unix-compat API uses FilePath.

setup :: IO FilePath
setup = do
  tmpDir <-
    (\s -> s </> "effects-fs" </> "unit")
      <$> Dir.getTemporaryDirectory

  exists <- Dir.doesDirectoryExist tmpDir
  when exists (Dir.removeDirectoryRecursive tmpDir)
  Dir.createDirectoryIfMissing True tmpDir

  pure tmpDir

teardown :: FilePath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> show fp
