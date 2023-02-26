module Main (main) where

import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removePathForcibly,
  )
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
        [ PathReader.tests,
          PathWriter.tests args
        ]

setup :: IO FilePath
setup = do
  tmpDir <- (</> "effects-fs/unit") <$> getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: FilePath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> fp
