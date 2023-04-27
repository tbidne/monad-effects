module Main (main) where

import Effects.FileSystem.Path (Path, (</>))
import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removeDirectoryRecursiveIfExists,
    removePathForcibly,
  )
import PathReader qualified
import PathWriter qualified
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)
import Utils qualified as U

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ PathReader.tests,
          PathWriter.tests args
        ]

setup :: IO Path
setup = do
  tmpDir <-
    (\s -> s </> U.strToPath "effects-fs" </> U.strToPath "unit")
      <$> getTemporaryDirectory
  removeDirectoryRecursiveIfExists tmpDir
  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: Path -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> U.pathToStr fp
