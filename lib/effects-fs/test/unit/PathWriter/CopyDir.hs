{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PathWriter.CopyDir (tests) where

import Control.Exception (IOException, displayException)
import Control.Exception.Utils (TextException, throwText)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bifunctor (first)
import Data.IORef (IORef)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Word (Word8)
import Effects.FileSystem.FileWriter
  ( OsPath,
  )
import Effects.FileSystem.PathReader
  ( MonadPathReader,
  )
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig),
    MonadPathWriter
      ( copyFileWithMetadata,
        createDirectory,
        removeDirectory,
        removeDirectoryRecursive,
        removeFile
      ),
    Overwrite (OverwriteAll, OverwriteDirectories, OverwriteNone),
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
    createDirectoryIfMissing,
  )
import Effects.FileSystem.PathWriter qualified as PW
import Effects.IORef (MonadIORef (modifyIORef', newIORef, readIORef))
import FileSystem.OsPath (osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import TestUtils qualified

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "copyDirectoryRecursive"
    [ overwriteTests getTmpDir,
      copyDirectoryRecursiveMiscTests getTmpDir
    ]

overwriteTests :: IO OsPath -> TestTree
overwriteTests getTmpDir =
  testGroup
    "Overwrite"
    [ cdrOverwriteNoneTests getTmpDir,
      cdrOverwriteTargetTests getTmpDir,
      cdrOverwriteAllTests getTmpDir
    ]

copyDirectoryRecursiveMiscTests :: IO OsPath -> TestTree
copyDirectoryRecursiveMiscTests getTmpDir =
  testGroup
    "Misc"
    [ copyTestData getTmpDir,
      copyDotDir getTmpDir,
      copyHidden getTmpDir,
      copyDirNoSrcException getTmpDir
    ]

copyTestData :: IO OsPath -> TestTree
copyTestData getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir

  let dataDir = tmpDir </> [osp|data|]
      srcDir = dataDir
      destDir = tmpDir </> [osp|copyTestData|]

  createDirectoryIfMissing False destDir

  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  TestUtils.assertFilesExist $
    (\p -> destDir </> [osp|data|] </> p)
      <$> [ [osp|.hidden|] </> [osp|f1|],
            [osp|bar|],
            [osp|baz|],
            [osp|foo|],
            [osp|dir1|] </> [osp|f|],
            [osp|dir2|] </> [osp|f|],
            [osp|dir3|] </> [osp|f|],
            [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|]
          ]
  TestUtils.assertDirsExist $
    (\p -> destDir </> [osp|data|] </> p)
      <$> [ [osp|.hidden|],
            [osp|dir1|],
            [osp|dir2|],
            [osp|dir3|],
            [osp|dir3|] </> [osp|dir3.1|]
          ]

  -- Notice that while the link names are copied to the new location, of course
  -- the _targets_ still refer to the old location (dataDir).
  TestUtils.assertSymlinksExistTarget $
    (\(l, t) -> (destDir </> [osp|data|] </> l, dataDir </> t))
      <$> [ ([osp|l1|], [osp|foo|]),
            ([osp|l2|], [osp|dir2|]),
            ([osp|l3|], [osp|bad|])
          ]
  where
    desc = "Copies test data directory with hidden dirs, symlinks"

copyDotDir :: IO OsPath -> TestTree
copyDotDir getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|copyDotDir|]

  let srcDir = tmpDir </> [osp|src-0.2.2|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir
  createDirectoryIfMissing False srcDir
  TestUtils.writeFiles [(srcDir </> [osp|f|], "")]

  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  TestUtils.assertDirsExist [destDir </> [osp|src-0.2.2|]]
  TestUtils.assertFilesExist [destDir </> [osp|src-0.2.2|] </> [osp|f|]]
  where
    desc = "Copies dir with dots in the name"

copyHidden :: IO OsPath -> TestTree
copyHidden getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|copyHidden|]

  let srcDir = tmpDir </> [osp|.hidden|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir
  createDirectoryIfMissing False srcDir
  TestUtils.writeFiles [(srcDir </> [osp|f|], "")]

  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  TestUtils.assertDirsExist [destDir </> [osp|.hidden|]]
  TestUtils.assertFilesExist [destDir </> [osp|.hidden|] </> [osp|f|]]
  where
    desc = "Copies top-level hidden dir"

copyDirNoSrcException :: IO OsPath -> TestTree
copyDirNoSrcException getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|copyDirNoSrcException|]

  let badSrc = tmpDir </> [osp|badSrc|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir

  let copy =
        PW.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          badSrc
          destDir

  try @_ @IOException copy >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"
  where
    desc = "Bad source throws exception"

cdrOverwriteNoneTests :: IO OsPath -> TestTree
cdrOverwriteNoneTests getTmpDir =
  testGroup
    "OverwriteNone"
    [ cdrnFresh getTmpDir,
      cdrnCustomTarget getTmpDir,
      cdrnDestNonExtantFails getTmpDir,
      cdrnOverwriteFails getTmpDir,
      cdrnPartialFails getTmpDir
    ]

cdrnFresh :: IO OsPath -> TestTree
cdrnFresh getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrnFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrnCustomTarget :: IO OsPath -> TestTree
cdrnCustomTarget getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrnCustomTarget|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]
      target = [osp|target|]

  createDirectoryIfMissing False destDir

  PW.copyDirectoryRecursiveConfig
    (MkCopyDirConfig OverwriteNone (TargetNameLiteral target))
    srcDir
    destDir

  assertSrcExists tmpDir
  TestUtils.assertFilesExist $
    (destDir </>)
      <$> [ [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|target|] </> [osp|a|] </> [osp|f2|],
            [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|target|] </> [osp|a|] </> [osp|f4|],
            [osp|target|] </> [osp|a|] </> [osp|f5|],
            [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  TestUtils.assertDirsExist $
    (destDir </>)
      <$> [ [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|target|] </> [osp|empty|] </> [osp|d|]
          ]
  where
    desc = "Copy with custom directory succeeds"

cdrnDestNonExtantFails :: IO OsPath -> TestTree
cdrnDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrnDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      PW.copyDirectoryRecursiveConfig
        (overwriteConfig OverwriteNone)
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  TestUtils.assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrnOverwriteFails :: IO OsPath -> TestTree
cdrnOverwriteFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrnOverwriteFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: This causes the expected error
  createDirectoryIfMissing False (destDir </> [osp|src|])

  -- copy files
  result <-
    try $
      PW.copyDirectoryRecursiveConfig
        (overwriteConfig OverwriteNone)
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  TestUtils.assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|],
            [osp|src|] </> [osp|empty|]
          ]
  where
    desc = "Copy to extant dest/<target> fails"
    suffix =
      FS.OsPath.decodeDisplayEx ([osp|dest|] </> [osp|src|])
        <> ": copyDirectoryNoOverwrite: already exists (Attempted directory overwrite when CopyDirConfig.overwrite is OverwriteNone)"

cdrnPartialFails :: IO OsPath -> TestTree
cdrnPartialFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrnPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runPartialIO $
        PW.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: TextException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  TestUtils.assertDirsDoNotExist [destDir </> [osp|src|]]
  where
    desc = "Partial failure rolls back changes"

cdrOverwriteTargetTests :: IO OsPath -> TestTree
cdrOverwriteTargetTests getTmpDir =
  testGroup
    "OverwriteDirectories"
    [ cdrtFresh getTmpDir,
      cdrtDestNonExtantFails getTmpDir,
      cdrtOverwriteTargetSucceeds getTmpDir,
      cdrtOverwriteTargetMergeSucceeds getTmpDir,
      cdrtOverwriteTargetMergeFails getTmpDir,
      cdrtOverwriteFileFails getTmpDir,
      cdrtPartialFails getTmpDir,
      cdrtOverwritePartialFails getTmpDir
    ]

cdrtFresh :: IO OsPath -> TestTree
cdrtFresh getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrtDestNonExtantFails :: IO OsPath -> TestTree
cdrtDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      PW.copyDirectoryRecursiveConfig (overwriteConfig OverwriteDirectories) srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  TestUtils.assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrtOverwriteTargetSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetSucceeds getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtOverwriteTargetSucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: test that dir already exists and succeeds
  createDirectoryIfMissing False (destDir </> [osp|src|])
  createDirectoryIfMissing False (destDir </> [osp|src|] </> [osp|test|])
  TestUtils.writeFiles [(destDir </> [osp|src|] </> [osp|test|] </> [osp|here|], "cat")]

  -- copy files
  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  assertSrcExists tmpDir
  TestUtils.assertFilesExist [destDir </> [osp|src|] </> [osp|test|] </> [osp|here|]]
  assertDestExists tmpDir
  where
    desc = "copy to extant dest/<target> succeeds"

cdrtOverwriteTargetMergeSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetMergeSucceeds getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeSucceeds|]
  let srcDir = tmpDir </> [osp|src|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True destDir
  createDirectoryIfMissing True srcDir

  -- NOTE: test that dir already exists and succeeds
  let d1 = destDir </> [osp|one|]
      d1Files = (d1 </>) <$> [[osp|f1|], [osp|f2|]]
      d2 = destDir </> [osp|two|]
      d2Files = (d2 </>) <$> [[osp|f1|], [osp|f2|]]

      s1 = srcDir </> [osp|one|]
      s1Files = (s1 </>) <$> [[osp|f3|], [osp|f4|]]
      s2 = srcDir </> [osp|two|]
      s2Files = (s2 </>) <$> [[osp|f3|], [osp|f4|]]

  createDirectoryIfMissing False d1
  createDirectoryIfMissing False d2
  createDirectoryIfMissing False s1
  createDirectoryIfMissing False s2
  TestUtils.writeFiles $
    map (,"cat") d1Files
      ++ map (,"cat") d2Files
      ++ map (,"cat") s1Files
      ++ map (,"cat") s2Files

  -- copy files
  PW.copyDirectoryRecursiveConfig
    config
    srcDir
    destDir

  -- assert copy correctly merged directories
  TestUtils.assertFilesExist $
    (destDir </>)
      <$> [ [osp|one|] </> [osp|f1|],
            [osp|one|] </> [osp|f2|],
            [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f1|],
            [osp|two|] </> [osp|f2|],
            [osp|two|] </> [osp|f3|],
            [osp|two|] </> [osp|f4|]
          ]

  -- src still exists
  TestUtils.assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f3|],
            [osp|two|] </> [osp|f4|]
          ]
  TestUtils.assertFilesDoNotExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f1|],
            [osp|one|] </> [osp|f2|],
            [osp|two|] </> [osp|f1|],
            [osp|two|] </> [osp|f2|]
          ]
  where
    desc = "copy to extant dest/<target> merges successfully"
    config = MkCopyDirConfig OverwriteDirectories TargetNameDest

cdrtOverwriteTargetMergeFails :: IO OsPath -> TestTree
cdrtOverwriteTargetMergeFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeFails|]
  let srcDir = tmpDir </> [osp|src|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True destDir
  createDirectoryIfMissing True srcDir

  -- NOTE: test that dir already exists and succeeds
  let d1 = destDir </> [osp|one|]
      d1Files = (d1 </>) <$> [[osp|f1|], [osp|f2|]]
      d2 = destDir </> [osp|two|]
      -- f3 introduces the collision failure we want
      d2Files = (d2 </>) <$> [[osp|f1|], [osp|f2|], [osp|f3|]]

      s1 = srcDir </> [osp|one|]
      s1Files = (s1 </>) <$> [[osp|f3|], [osp|f4|]]
      s2 = srcDir </> [osp|two|]
      s2Files = (s2 </>) <$> [[osp|f3|], [osp|f4|]]

  createDirectoryIfMissing False d1
  createDirectoryIfMissing False d2
  createDirectoryIfMissing False s1
  createDirectoryIfMissing False s2
  TestUtils.writeFiles $
    map (,"cat") d1Files
      ++ map (,"cat") d2Files
      ++ map (,"cat") s1Files
      ++ map (,"cat") s2Files

  -- copy files
  result <-
    try $
      PW.copyDirectoryRecursiveConfig
        config
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert dest unchanged from bad copy
  TestUtils.assertFilesExist $
    (destDir </>)
      <$> [ [osp|one|] </> [osp|f1|],
            [osp|one|] </> [osp|f2|],
            [osp|two|] </> [osp|f1|],
            [osp|two|] </> [osp|f2|],
            [osp|two|] </> [osp|f3|]
          ]

  TestUtils.assertFilesDoNotExist $
    (destDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f4|]
          ]

  -- src still exists
  TestUtils.assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f3|],
            [osp|two|] </> [osp|f4|]
          ]
  TestUtils.assertFilesDoNotExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f1|],
            [osp|one|] </> [osp|f2|],
            [osp|two|] </> [osp|f1|],
            [osp|two|] </> [osp|f2|]
          ]
  where
    desc = "copy to extant dest/<target> merge fails"
    config = MkCopyDirConfig OverwriteDirectories TargetNameDest
    suffix =
      FS.OsPath.decodeDisplayEx ([osp|dest|] </> [osp|two|] </> [osp|f3|])
        <> ": copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtOverwriteFileFails :: IO OsPath -> TestTree
cdrtOverwriteFileFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtOverwriteFileFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True (destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|])

  -- NOTE: this line causes it to die
  TestUtils.writeFiles [(destDir </> pathEnd, "cat")]

  -- copy files
  result <-
    try $
      PW.copyDirectoryRecursiveConfig
        (overwriteConfig OverwriteDirectories)
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir
  TestUtils.assertFilesExist [destDir </> pathEnd]
  where
    desc = "copy to extant dest/<target>/file fails"
    pathEnd = [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|]
    suffix =
      FS.OsPath.decodeDisplayEx ([osp|dest|] </> pathEnd)
        <> ": copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtPartialFails :: IO OsPath -> TestTree
cdrtPartialFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runPartialIO $
        PW.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: TextException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  TestUtils.assertDirsDoNotExist [destDir </> [osp|src|]]
  where
    desc = "Partial failure rolls back changes"

cdrtOverwritePartialFails :: IO OsPath -> TestTree
cdrtOverwritePartialFails getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdrtOverwritePartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: test overwriting
  createDirectoryIfMissing False (destDir </> [osp|src|])
  createDirectoryIfMissing False (destDir </> [osp|src|] </> [osp|test|])
  TestUtils.writeFiles [(destDir </> [osp|src|] </> [osp|test|] </> [osp|here|], "cat")]

  -- copy files
  result <-
    try $
      runPartialIO $
        PW.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: TextException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were not copied over
  TestUtils.assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|],
            [osp|src|] </> [osp|empty|]
          ]

  -- assert original file exists after copy failure
  TestUtils.assertFilesExist [destDir </> [osp|src|] </> [osp|test|] </> [osp|here|]]
  where
    desc = "Partial failure with extant dest/<target> rolls back changes"

cdrOverwriteAllTests :: IO OsPath -> TestTree
cdrOverwriteAllTests getTmpDir =
  testGroup
    "OverwriteAll"
    [ cdraOverWriteFilesucceeds getTmpDir
    ]

cdraOverWriteFilesucceeds :: IO OsPath -> TestTree
cdraOverWriteFilesucceeds getTmpDir = testCase desc $ do
  tmpDir <- TestUtils.mkTestPath getTmpDir [osp|cdraOverTestUtils.writeFilesucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True (destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|])

  -- NOTE: this line is what is tested
  TestUtils.writeFiles
    [ ( destDir
          </> [osp|src|]
          </> [osp|a|]
          </> [osp|b|]
          </> [osp|c|]
          </> [osp|f1|],
        "cat"
      )
    ]
  TestUtils.assertFileContents
    [ ( destDir
          </> [osp|src|]
          </> [osp|a|]
          </> [osp|b|]
          </> [osp|c|]
          </> [osp|f1|],
        "cat"
      )
    ]

  -- copy files
  PW.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteAll)
    srcDir
    destDir

  assertSrcExists tmpDir
  -- check contents actually overwritten
  TestUtils.assertFileContents
    [ ( destDir
          </> [osp|src|]
          </> [osp|a|]
          </> [osp|b|]
          </> [osp|c|]
          </> [osp|f1|],
        "1"
      )
    ]
  assertDestExists tmpDir
  where
    desc = "Copy to extant dest/<target>/file succeeds"

-------------------------------------------------------------------------------
--                                  Setup                                    --
-------------------------------------------------------------------------------

setupSrc :: (HasCallStack) => OsPath -> IO OsPath
setupSrc baseDir = do
  let files =
        [ [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
          [osp|a|] </> [osp|f2|],
          [osp|a|] </> [osp|b|] </> [osp|f3|],
          [osp|a|] </> [osp|f4|],
          [osp|a|] </> [osp|f5|],
          [osp|a|] </> [osp|b|] </> [osp|f5|]
        ]
      srcDir = baseDir </> [osp|src|]

  -- create directories and files
  createDirectoryIfMissing True (srcDir </> [osp|a|] </> [osp|b|] </> [osp|c|])
  createDirectoryIfMissing True (srcDir </> [osp|empty|] </> [osp|d|])

  let baseFiles = zip files ["1", "2", "3", "4", "5", "6"]
      srcFiles = fmap (first (srcDir </>)) baseFiles

  TestUtils.writeFiles srcFiles
  assertSrcExists baseDir
  pure srcDir

overwriteConfig :: Overwrite -> CopyDirConfig
overwriteConfig ow = MkCopyDirConfig ow TargetNameSrc

-------------------------------------------------------------------------------
--                                  Mock                                     --
-------------------------------------------------------------------------------

newtype PartialIO a = MkPartialT (ReaderT (IORef Word8) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadThrow,
      MonadCatch,
      MonadPathReader
    )
    via (ReaderT (IORef Word8) IO)

runPartialIO :: PartialIO a -> IO a
runPartialIO (MkPartialT x) = do
  ref <- newIORef 0
  runReaderT x ref

instance MonadPathWriter PartialIO where
  createDirectory = liftIO . createDirectory
  createDirectoryIfMissing b = liftIO . createDirectoryIfMissing b
  removeDirectoryRecursive = liftIO . removeDirectoryRecursive
  removeDirectory = liftIO . removeDirectory
  removeFile = liftIO . removeFile

  -- so we can test failures
  copyFileWithMetadata src dest = do
    counterRef <- MkPartialT ask
    counter <- readIORef counterRef

    -- want some successes first
    if counter > 3
      then throwText $ "Failed copying: " <> T.pack (show dest)
      else do
        modifyIORef' counterRef (+ 1)
        liftIO $ copyFileWithMetadata src dest

-------------------------------------------------------------------------------
--                                Assertions                                 --
-------------------------------------------------------------------------------

-- | Asserts that the src directory created by 'setupSrc' exists at the given
-- location.
assertSrcExists :: (HasCallStack) => OsPath -> IO ()
assertSrcExists baseDir = do
  let srcDir = baseDir </> [osp|src|]
  TestUtils.assertFilesExist $
    (srcDir </>)
      <$> [ [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|a|] </> [osp|f2|],
            [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|a|] </> [osp|f4|],
            [osp|a|] </> [osp|f5|],
            [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  TestUtils.assertDirsExist $
    (srcDir </>)
      <$> [ [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|empty|] </> [osp|d|]
          ]

-- | Asserts that the copied dest directory exists.
assertDestExists :: (HasCallStack) => OsPath -> IO ()
assertDestExists baseDir = do
  let destDir = baseDir </> [osp|dest|]
  TestUtils.assertFilesExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|src|] </> [osp|a|] </> [osp|f2|],
            [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|src|] </> [osp|a|] </> [osp|f4|],
            [osp|src|] </> [osp|a|] </> [osp|f5|],
            [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  TestUtils.assertDirsExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|src|] </> [osp|empty|] </> [osp|d|]
          ]
