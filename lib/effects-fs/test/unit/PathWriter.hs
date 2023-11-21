{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module PathWriter (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef (IORef)
import Data.List qualified as L
import Data.Word (Word8)
import Effects.Exception
  ( HasCallStack,
    IOException,
    MonadCatch,
    MonadMask,
    MonadThrow,
    StringException,
    displayException,
    throwString,
    tryCS,
  )
import Effects.FileSystem.FileReader (MonadFileReader (readBinaryFile))
import Effects.FileSystem.FileWriter
  ( MonadFileWriter (writeBinaryFile),
    OsPath,
  )
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathReader
  ( MonadPathReader,
    doesDirectoryExist,
    doesFileExist,
    doesSymbolicLinkExist,
    getSymbolicLinkTarget,
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
import Effects.FileSystem.PathWriter qualified as PathWriter
import Effects.FileSystem.Utils (osp, (</>))
import Effects.FileSystem.Utils qualified as Utils
import Effects.IORef (MonadIORef (modifyIORef', newIORef, readIORef))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO OsPath -> TestTree
tests getTmpDir = do
  testGroup
    "PathWriter"
    [ copyDirectoryRecursiveTests getTmpDir,
      removeLinkTests getTmpDir,
      copyLinkTests getTmpDir,
      removeExistsTests getTmpDir
    ]

copyDirectoryRecursiveTests :: IO OsPath -> TestTree
copyDirectoryRecursiveTests getTmpDir =
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

  let srcDir = dataDir
      destDir = tmpDir </> [osp|copyTestData|]

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  assertFilesExist $
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
  assertDirsExist $
    (\p -> destDir </> [osp|data|] </> p)
      <$> [ [osp|.hidden|],
            [osp|dir1|],
            [osp|dir2|],
            [osp|dir3|],
            [osp|dir3|] </> [osp|dir3.1|]
          ]
  assertSymlinksExistTarget $
    (\(l, t) -> (destDir </> [osp|data|] </> l, t))
      <$> [ ([osp|l1|], [osp|foo|]),
            ([osp|l2|], [osp|dir2|]),
            ([osp|l3|], [osp|bad|])
          ]
  where
    desc = "Copies test data directory with hidden dirs, symlinks"
    dataDir = [osp|test|] </> [osp|data|]

copyDotDir :: IO OsPath -> TestTree
copyDotDir getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyDotDir|]

  let srcDir = tmpDir </> [osp|src-0.2.2|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir
  createDirectoryIfMissing False srcDir
  writeFiles [(srcDir </> [osp|f|], "")]

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  assertDirsExist [destDir </> [osp|src-0.2.2|]]
  assertFilesExist [destDir </> [osp|src-0.2.2|] </> [osp|f|]]
  where
    desc = "Copies dir with dots in the name"

copyHidden :: IO OsPath -> TestTree
copyHidden getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyHidden|]

  let srcDir = tmpDir </> [osp|.hidden|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir
  createDirectoryIfMissing False srcDir
  writeFiles [(srcDir </> [osp|f|], "")]

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  assertDirsExist [destDir </> [osp|.hidden|]]
  assertFilesExist [destDir </> [osp|.hidden|] </> [osp|f|]]
  where
    desc = "Copies top-level hidden dir"

copyDirNoSrcException :: IO OsPath -> TestTree
copyDirNoSrcException getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyDirNoSrcException|]

  let badSrc = tmpDir </> [osp|badSrc|]
      destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False tmpDir
  createDirectoryIfMissing False destDir

  let copy =
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          badSrc
          destDir

  tryCS @_ @IOException copy >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected PathNotFoundException"
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
  tmpDir <- mkTestPath getTmpDir [osp|cdrnFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrnCustomTarget :: IO OsPath -> TestTree
cdrnCustomTarget getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnCustomTarget|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]
      target = [osp|target|]

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (MkCopyDirConfig OverwriteNone (TargetNameLiteral target))
    srcDir
    destDir

  assertSrcExists tmpDir
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|target|] </> [osp|a|] </> [osp|f2|],
            [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|target|] </> [osp|a|] </> [osp|f4|],
            [osp|target|] </> [osp|a|] </> [osp|f5|],
            [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ [osp|target|] </> [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|target|] </> [osp|empty|] </> [osp|d|]
          ]
  where
    desc = "Copy with custom directory succeeds"

cdrnDestNonExtantFails :: IO OsPath -> TestTree
cdrnDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
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
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrnOverwriteFails :: IO OsPath -> TestTree
cdrnOverwriteFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnOverwriteFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: This causes the expected error
  createDirectoryIfMissing False (destDir </> [osp|src|])

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
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
  assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|],
            [osp|src|] </> [osp|empty|]
          ]
  where
    desc = "Copy to extant dest/<target> fails"
    suffix =
      Utils.decodeOsToFpDisplayEx ([osp|dest|] </> [osp|src|])
        <> ": copyDirectoryNoOverwrite: already exists (Attempted directory overwrite when CopyDirConfig.overwrite is OverwriteNone)"

cdrnPartialFails :: IO OsPath -> TestTree
cdrnPartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  assertDirsDoNotExist [destDir </> [osp|src|]]
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
  tmpDir <- mkTestPath getTmpDir [osp|cdrtFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrtDestNonExtantFails :: IO OsPath -> TestTree
cdrtDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig (overwriteConfig OverwriteDirectories) srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrtOverwriteTargetSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetSucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: test that dir already exists and succeeds
  createDirectoryIfMissing False (destDir </> [osp|src|])
  createDirectoryIfMissing False (destDir </> [osp|src|] </> [osp|test|])
  writeFiles [(destDir </> [osp|src|] </> [osp|test|] </> [osp|here|], "cat")]

  -- copy files
  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteDirectories)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertFilesExist [destDir </> [osp|src|] </> [osp|test|] </> [osp|here|]]
  assertDestExists tmpDir
  where
    desc = "copy to extant dest/<target> succeeds"

cdrtOverwriteTargetMergeSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetMergeSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeSucceeds|]
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
  writeFiles $
    (map (,"cat") d1Files)
      ++ (map (,"cat") d2Files)
      ++ (map (,"cat") s1Files)
      ++ (map (,"cat") s2Files)

  -- copy files
  PathWriter.copyDirectoryRecursiveConfig
    config
    srcDir
    destDir

  -- assert copy correctly merged directories
  assertFilesExist $
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
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f3|],
            [osp|two|] </> [osp|f4|]
          ]
  assertFilesDoNotExist $
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
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeFails|]
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
  writeFiles $
    (map (,"cat") d1Files)
      ++ (map (,"cat") d2Files)
      ++ (map (,"cat") s1Files)
      ++ (map (,"cat") s2Files)

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
        config
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert dest unchanged from bad copy
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|one|] </> [osp|f1|],
            [osp|one|] </> [osp|f2|],
            [osp|two|] </> [osp|f1|],
            [osp|two|] </> [osp|f2|],
            [osp|two|] </> [osp|f3|]
          ]

  assertFilesDoNotExist $
    (destDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f4|]
          ]

  -- src still exists
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one|] </> [osp|f3|],
            [osp|one|] </> [osp|f4|],
            [osp|two|] </> [osp|f3|],
            [osp|two|] </> [osp|f4|]
          ]
  assertFilesDoNotExist $
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
      Utils.decodeOsToFpDisplayEx ([osp|dest|] </> [osp|two|] </> [osp|f3|])
        <> ": copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtOverwriteFileFails :: IO OsPath -> TestTree
cdrtOverwriteFileFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteFileFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True (destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|])

  -- NOTE: this line causes it to die
  writeFiles [(destDir </> pathEnd, "cat")]

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
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
  assertFilesExist [destDir </> pathEnd]
  where
    desc = "copy to extant dest/<target>/file fails"
    pathEnd = [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|]
    suffix =
      Utils.decodeOsToFpDisplayEx ([osp|dest|] </> pathEnd)
        <> ": copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtPartialFails :: IO OsPath -> TestTree
cdrtPartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  assertDirsDoNotExist [destDir </> [osp|src|]]
  where
    desc = "Partial failure rolls back changes"

cdrtOverwritePartialFails :: IO OsPath -> TestTree
cdrtOverwritePartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwritePartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing False destDir

  -- NOTE: test overwriting
  createDirectoryIfMissing False (destDir </> [osp|src|])
  createDirectoryIfMissing False (destDir </> [osp|src|] </> [osp|test|])
  writeFiles [(destDir </> [osp|src|] </> [osp|test|] </> [osp|here|], "cat")]

  -- copy files
  result <-
    tryCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were not copied over
  assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|],
            [osp|src|] </> [osp|empty|]
          ]

  -- assert original file exists after copy failure
  assertFilesExist [destDir </> [osp|src|] </> [osp|test|] </> [osp|here|]]
  where
    desc = "Partial failure with extant dest/<target> rolls back changes"

cdrOverwriteAllTests :: IO OsPath -> TestTree
cdrOverwriteAllTests getTmpDir =
  testGroup
    "OverwriteAll"
    [ cdraOverwriteFileSucceeds getTmpDir
    ]

cdraOverwriteFileSucceeds :: IO OsPath -> TestTree
cdraOverwriteFileSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdraOverwriteFileSucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True (destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|])

  -- NOTE: this line is what is tested
  writeFiles
    [ ( destDir
          </> [osp|src|]
          </> [osp|a|]
          </> [osp|b|]
          </> [osp|c|]
          </> [osp|f1|],
        "cat"
      )
    ]
  assertFileContents
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
  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteAll)
    srcDir
    destDir

  assertSrcExists tmpDir
  -- check contents actually overwritten
  assertFileContents
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

removeLinkTests :: IO OsPath -> TestTree
removeLinkTests getTestDir =
  testGroup
    "removeSymbolicLink"
    [ removeSymbolicLinkFileLink getTestDir,
      removeSymbolicLinkFileException getTestDir,
      removeSymbolicLinkBadException getTestDir
    ]

removeSymbolicLinkFileLink :: IO OsPath -> TestTree
removeSymbolicLinkFileLink getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]

  assertSymlinksExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]

  PW.removeSymbolicLink (testDir </> [osp|file-link|])
  PW.removeSymbolicLink (testDir </> [osp|dir-link|])

  assertSymlinksDoNotExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]
  where
    desc = "Removes symbolic links"

removeSymbolicLinkFileException :: IO OsPath -> TestTree
removeSymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]
  let filePath = testDir </> [osp|file|]

  assertFilesExist [filePath]

  tryCS @_ @IOException (PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"

  assertFilesExist [filePath]
  where
    desc = "Exception for file"

removeSymbolicLinkBadException :: IO OsPath -> TestTree
removeSymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkBadException|]
  let filePath = testDir </> [osp|bad-path|]

  tryCS @_ @IOException (PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"
  where
    desc = "Exception for bad path"

copyLinkTests :: IO OsPath -> TestTree
copyLinkTests getTestDir =
  testGroup
    "copySymbolicLink"
    [ copySymbolicLinks getTestDir,
      copySymbolicLinkFileException getTestDir,
      copySymbolicLinkDirException getTestDir,
      copySymbolicLinkBadException getTestDir
    ]

copySymbolicLinks :: IO OsPath -> TestTree
copySymbolicLinks getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copyFileLink|]
  let srcFile = testDir </> [osp|file-link|]
      srcDir = testDir </> [osp|dir-link|]
      destFile = testDir </> [osp|file-link2|]
      destDir = testDir </> [osp|dir-link2|]

  assertSymlinksExist [srcFile, srcDir]
  assertSymlinksDoNotExist [destFile, destDir]

  PW.copySymbolicLink srcFile destFile
  PW.copySymbolicLink srcDir destDir

  assertSymlinksExistTarget
    [ (srcFile, testDir </> [osp|file|]),
      (destFile, testDir </> [osp|file|]),
      (srcDir, testDir </> [osp|dir|]),
      (destDir, testDir </> [osp|dir|])
    ]
  where
    desc = "Copies symbolic links"

copySymbolicLinkFileException :: IO OsPath -> TestTree
copySymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkFileException|]
  let src = testDir </> [osp|file|]
      dest = testDir </> [osp|dest|]
  tryCS @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"

copySymbolicLinkDirException :: IO OsPath -> TestTree
copySymbolicLinkDirException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkDirException|]
  let src = testDir </> [osp|dir|]
      dest = testDir </> [osp|dest|]
  tryCS @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for directory"

copySymbolicLinkBadException :: IO OsPath -> TestTree
copySymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkBadException|]
  let src = testDir </> [osp|bad-path|]
      dest = testDir </> [osp|dest|]
  tryCS @_ @IOException (PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"

-- NOTE: For removeExistsTests, we do not test all permutations. In particular,
-- we do not test symlinks as "bad paths" for e.g. removeFileIfExists or
-- removeDirIfExists because those functions are based on
-- does(file|directory)Exist, and those return True based on the _target_
-- for the link.
--
-- In other words, for an extant directory link, doesDirectoryExist will return
-- true, yet removeDirectory will throw an exception.
--
-- doesFileExist / removeFile will work on Posix because Posix treats symlinks
-- as files...but it wil fail on windows.
--
-- But we want to keep these functions as simple as possible i.e. the obvious
-- doesXExist -> removeX. So we don't maintain any illusion that these
-- functions are total for all possible path type inputs. Really you should
-- only use them when you know the type of your potential path.

removeExistsTests :: IO OsPath -> TestTree
removeExistsTests getTestDir =
  testGroup
    "removeXIfExists"
    [ removeFileIfExistsTrue getTestDir,
      removeFileIfExistsFalseBad getTestDir,
      removeFileIfExistsFalseWrongType getTestDir,
      removeDirIfExistsTrue getTestDir,
      removeDirIfExistsFalseBad getTestDir,
      removeDirIfExistsFalseWrongType getTestDir,
      removeSymlinkIfExistsTrue getTestDir,
      removeSymlinkIfExistsFalseBad getTestDir,
      removeSymlinkIfExistsFalseWrongType getTestDir
    ]

removeFileIfExistsTrue :: IO OsPath -> TestTree
removeFileIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsTrue|]
  let file = testDir </> [osp|file|]
  assertFilesExist [file]

  PW.removeFileIfExists file

  assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists removes file"

removeFileIfExistsFalseBad :: IO OsPath -> TestTree
removeFileIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsFalseBad|]
  let file = testDir </> [osp|bad-path|]
  assertFilesDoNotExist [file]

  PW.removeFileIfExists file

  assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists does nothing for bad path"

removeFileIfExistsFalseWrongType :: IO OsPath -> TestTree
removeFileIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsFalseWrongType|]
  let dir = testDir </> [osp|dir|]

  assertDirsExist [dir]

  PW.removeFileIfExists dir

  assertDirsExist [dir]
  where
    desc = "removeFileIfExists does nothing for wrong file types"

removeDirIfExistsTrue :: IO OsPath -> TestTree
removeDirIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsTrue|]
  let dir = testDir </> [osp|dir|]
  assertDirsExist [dir]

  PW.removeDirectoryIfExists dir

  assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists removes dir"

removeDirIfExistsFalseBad :: IO OsPath -> TestTree
removeDirIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsFalseBad|]
  let dir = testDir </> [osp|bad-path|]
  assertDirsDoNotExist [dir]

  PW.removeDirectoryIfExists dir

  assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists does nothing for bad path"

removeDirIfExistsFalseWrongType :: IO OsPath -> TestTree
removeDirIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]

  assertFilesExist [file]

  PW.removeDirectoryIfExists file

  assertFilesExist [file]
  where
    desc = "removeDirectoryIfExists does nothing for wrong file types"

removeSymlinkIfExistsTrue :: IO OsPath -> TestTree
removeSymlinkIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsTrue|]
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]

  assertSymlinksExist [fileLink, dirLink]

  PW.removeSymbolicLinkIfExists fileLink
  PW.removeSymbolicLinkIfExists dirLink

  assertSymlinksDoNotExist [fileLink, dirLink]
  where
    desc = "removeSymbolicLinkIfExists removes links"

removeSymlinkIfExistsFalseBad :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsFalseBad|]
  let link = testDir </> [osp|bad-path|]
  assertSymlinksDoNotExist [link]

  PW.removeSymbolicLinkIfExists link

  assertSymlinksDoNotExist [link]
  where
    desc = "removeSymbolicLinkIfExists does nothing for bad path"

removeSymlinkIfExistsFalseWrongType :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  assertFilesExist [file]
  assertDirsExist [dir]

  PW.removeSymbolicLinkIfExists file
  PW.removeSymbolicLinkIfExists dir

  assertFilesExist [file]
  assertDirsExist [dir]
  where
    desc = "removeSymbolicLinkIfExists does nothing for wrong file types"

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

  writeFiles srcFiles
  assertSrcExists baseDir
  pure srcDir

writeFiles :: (HasCallStack) => [(OsPath, ByteString)] -> IO ()
writeFiles = traverse_ (uncurry writeBinaryFile)

overwriteConfig :: Overwrite -> CopyDirConfig
overwriteConfig ow = MkCopyDirConfig ow TargetNameSrc

setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|path-writer|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  PW.createDirectoryIfMissing True dir
  FW.writeBinaryFile file ""
  PW.createFileLink file fileLink
  PW.createDirectoryLink dir dirLink

  pure testDir

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
      then throwString $ "Failed copying: " <> show dest
      else do
        modifyIORef' counterRef (+ 1)
        liftIO $ copyFileWithMetadata src dest

-------------------------------------------------------------------------------
--                                Assertions                                 --
-------------------------------------------------------------------------------

assertSrcExists :: (HasCallStack) => OsPath -> IO ()
assertSrcExists baseDir = do
  let srcDir = baseDir </> [osp|src|]
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|a|] </> [osp|f2|],
            [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|a|] </> [osp|f4|],
            [osp|a|] </> [osp|f5|],
            [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  assertDirsExist $
    (srcDir </>)
      <$> [ [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|empty|] </> [osp|d|]
          ]

assertDestExists :: (HasCallStack) => OsPath -> IO ()
assertDestExists baseDir = do
  let destDir = baseDir </> [osp|dest|]
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|],
            [osp|src|] </> [osp|a|] </> [osp|f2|],
            [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|f3|],
            [osp|src|] </> [osp|a|] </> [osp|f4|],
            [osp|src|] </> [osp|a|] </> [osp|f5|],
            [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|f5|]
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|],
            [osp|src|] </> [osp|empty|] </> [osp|d|]
          ]

assertFilesExist :: (HasCallStack) => [OsPath] -> IO ()
assertFilesExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> Utils.decodeOsToFpShow p) exists

assertFilesDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertFilesDoNotExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file not to exist: " <> Utils.decodeOsToFpShow p) (not exists)

assertSymlinksExist :: (HasCallStack) => [OsPath] -> IO ()
assertSymlinksExist = assertSymlinksExist' . fmap (,Nothing)

assertSymlinksExistTarget :: (HasCallStack) => [(OsPath, OsPath)] -> IO ()
assertSymlinksExistTarget = assertSymlinksExist' . (fmap . fmap) Just

assertSymlinksExist' :: (HasCallStack) => [(OsPath, Maybe OsPath)] -> IO ()
assertSymlinksExist' = traverse_ $ \(l, t) -> do
  exists <- doesSymbolicLinkExist l
  assertBool ("Expected symlink to exist: " <> Utils.decodeOsToFpShow l) exists

  case t of
    Nothing -> pure ()
    Just expectedTarget -> do
      target <- getSymbolicLinkTarget l
      expectedTarget @=? target

assertSymlinksDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertSymlinksDoNotExist = traverse_ $ \l -> do
  exists <- doesSymbolicLinkExist l
  assertBool ("Expected symlink not to exist: " <> Utils.decodeOsToFpShow l) (not exists)

assertFileContents :: (HasCallStack) => [(OsPath, ByteString)] -> IO ()
assertFileContents = traverse_ $ \(p, expected) -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> Utils.decodeOsToFpShow p) exists
  actual <- readBinaryFile p
  expected @=? actual

assertDirsExist :: (HasCallStack) => [OsPath] -> IO ()
assertDirsExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory to exist: " <> Utils.decodeOsToFpShow p) exists

assertDirsDoNotExist :: (HasCallStack) => [OsPath] -> IO ()
assertDirsDoNotExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory not to exist: " <> Utils.decodeOsToFpShow p) (not exists)

mkTestPath :: IO OsPath -> OsPath -> IO OsPath
mkTestPath getPath s = do
  p <- getPath
  pure $ p </> s
