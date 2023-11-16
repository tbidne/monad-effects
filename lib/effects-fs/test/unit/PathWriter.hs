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
    PathDoesNotExistException,
    PathExistsException,
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
    createDirectoryIfMissing,
  )
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
    [ copyDirectoryRecursiveTests getTmpDir
    ]

copyDirectoryRecursiveTests :: IO OsPath -> TestTree
copyDirectoryRecursiveTests getTmpDir =
  testGroup
    "copyDirectoryRecursive"
    [ cdrOverwriteNoneTests getTmpDir,
      cdrOverwriteTargetTests getTmpDir,
      cdrOverwriteAllTests getTmpDir,
      copyTestData getTmpDir,
      copyDotDir getTmpDir,
      copyHidden getTmpDir
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
  assertSymlinksExist $
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
    Left (ex :: PathDoesNotExistException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path does not exist:" `L.isPrefixOf` exText)
  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix =
      "effects-fs"
        `cfp` "unit"
        `cfp` "cdrnDestNonExtantFails"
        `cfp` "dest"

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
    Left (ex :: PathExistsException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path already exists:" `L.isPrefixOf` exText)
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
      "effects-fs"
        `cfp` "unit"
        `cfp` "cdrnOverwriteFails"
        `cfp` "dest"
        `cfp` "src"

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
    Left (ex :: PathDoesNotExistException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path does not exist:" `L.isPrefixOf` exText)
  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix =
      "effects-fs"
        `cfp` "unit"
        `cfp` "cdrtDestNonExtantFails"
        `cfp` "dest"

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
    Left (ex :: PathExistsException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path already exists:" `L.isPrefixOf` exText)
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
      "effects-fs"
        `cfp` "unit"
        `cfp` "cdrtOverwriteTargetMergeFails"
        `cfp` "dest"
        `cfp` "two"
        `cfp` "f3"

cdrtOverwriteFileFails :: IO OsPath -> TestTree
cdrtOverwriteFileFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteFileFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  createDirectoryIfMissing True (destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|])

  -- NOTE: this line causes it to die
  writeFiles [(destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|], "cat")]

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
        (overwriteConfig OverwriteDirectories)
        srcDir
        destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: PathExistsException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path already exists:" `L.isPrefixOf` exText)
  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir
  assertFilesExist [destDir </> [osp|src|] </> [osp|a|] </> [osp|b|] </> [osp|c|] </> [osp|f1|]]
  where
    desc = "copy to extant dest/<target>/file fails"
    suffix =
      "effects-fs"
        `cfp` "unit"
        `cfp` "cdrtOverwriteFileFails"
        `cfp` "dest"
        `cfp` "src"
        `cfp` "a"
        `cfp` "b"
        `cfp` "c"
        `cfp` "f1"

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

assertSymlinksExist :: (HasCallStack) => [(OsPath, OsPath)] -> IO ()
assertSymlinksExist = traverse_ $ \(l, t) -> do
  exists <- doesSymbolicLinkExist l
  assertBool ("Expected symlink to exist: " <> Utils.decodeOsToFpShow l) exists

  target <- getSymbolicLinkTarget l
  t @=? target

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

cfp :: FilePath -> FilePath -> FilePath
cfp = Utils.combineFilePaths
