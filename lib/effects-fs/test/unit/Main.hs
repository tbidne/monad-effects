{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Foldable (for_)
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathReader (getTemporaryDirectory)
import Effects.FileSystem.PathWriter
  ( createDirectoryIfMissing,
    removeDirectoryRecursiveIfExists,
    removePathForcibly,
  )
import Effects.FileSystem.PathWriter qualified as PW
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

  createDataDir tmpDir

  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing = putStrLn $ "*** Not cleaning up tmp dir: " <> Utils.decodeOsToFpShow fp

-- | This is what we want to create:
--
-- @
-- λ. tree -a
-- .
-- ├── bar
-- ├── baz
-- ├── dir1
-- │   └── f
-- ├── dir2
-- │   └── f
-- ├── dir3
-- │   ├── dir3.1
-- │   │   └── f
-- │   └── f
-- ├── foo
-- ├── .hidden
-- │   └── f1
-- ├── l1 -> foo
-- ├── l2 -> dir2
-- └── l3 -> bad
--
-- 7 directories, 10 files
-- @
--
-- Originally we actually had this test data directory committed, but
-- unfortunately the bad sym link (l3 -> bad, which we want!) caused stack
-- to die when checking out this repo during a build. Thus we build the
-- needed directory during the test itself.
createDataDir :: OsPath -> IO ()
createDataDir tmpDir = do
  PW.removeDirectoryIfExists dataDir
  PW.createDirectoryIfMissing True dataDir

  createDirs
    [ [osp|dir1|],
      [osp|dir2|],
      [osp|dir3|],
      [osp|dir3|] </> [osp|dir3.1|],
      [osp|.hidden|]
    ]

  createFiles
    [ [osp|bar|],
      [osp|baz|],
      [osp|dir1|] </> [osp|f|],
      [osp|dir2|] </> [osp|f|],
      [osp|dir3|] </> [osp|f|],
      [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
      [osp|foo|],
      [osp|.hidden|] </> [osp|f1|]
    ]

  createSymlinks
    [ ([osp|l1|], [osp|foo|], False),
      ([osp|l2|], [osp|dir2|], True),
      ([osp|l3|], [osp|bad|], False)
    ]
  where
    dataDir = tmpDir </> [osp|data|]

    createDirs ds =
      for_ ds $ \p -> PW.createDirectory (dataDir </> p)

    createFiles ps =
      for_ ps $ \p -> FW.writeBinaryFile (dataDir </> p) ""

    createSymlinks ls =
      for_ ls $ \(n, t, isDir) ->
        if isDir
          then PW.createDirectoryLink (dataDir </> t) (dataDir </> n)
          else PW.createFileLink (dataDir </> t) (dataDir </> n)
