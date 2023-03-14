{-# LANGUAGE CPP #-}
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
    Path,
  )
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader
  ( MonadPathReader,
    doesDirectoryExist,
    doesFileExist,
  )
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (..),
    MonadPathWriter (..),
    Overwrite (..),
    PathDoesNotExistException,
    PathExistsException,
    createDirectoryIfMissing,
  )
import Effects.FileSystem.PathWriter qualified as PathWriter
import Effects.IORef (MonadIORef (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))

tests :: IO FilePath -> TestTree
tests getTmpDir = do
  testGroup
    "PathWriter"
    [ copyDirectoryRecursiveTests getTmpDir
    ]

copyDirectoryRecursiveTests :: IO FilePath -> TestTree
copyDirectoryRecursiveTests getTmpDir =
  testGroup
    "copyDirectoryRecursive"
    [ cdrOverwriteNoneTests getTmpDir,
      cdrOverwriteTargetTests getTmpDir,
      cdrOverwriteAllTests getTmpDir
    ]

cdrOverwriteNoneTests :: IO FilePath -> TestTree
cdrOverwriteNoneTests getTmpDir =
  testGroup
    "OverwriteNone"
    [ cdrnFresh getTmpDir,
      cdrnCustomTarget getTmpDir,
      cdrnDestNonExtantFails getTmpDir,
      cdrnOverwriteFails getTmpDir,
      cdrnPartialFails getTmpDir
    ]

cdrnFresh :: IO FilePath -> TestTree
cdrnFresh getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrnFresh") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteNone)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrnCustomTarget :: IO FilePath -> TestTree
cdrnCustomTarget getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrnCustomTarget") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"
      target = "target"

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (MkCopyDirConfig OverwriteNone (Just target))
    srcDir
    destDir

  assertSrcExists tmpDir
  assertFilesExist $
    (destDir </>)
      <$> [ "target/a/b/c/f1",
            "target/a/f2",
            "target/a/b/f3",
            "target/a/f4",
            "target/a/f5",
            "target/a/b/f5"
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ "target/a/b/c",
            "target/empty/d"
          ]
  where
    desc = "Copy with custom directory succeeds"

cdrnDestNonExtantFails :: IO FilePath -> TestTree
cdrnDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrnDestNonExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

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
#if !WINDOWS
    suffix = "effects-fs/unit/cdrnDestNonExtantFails/dest"
#else
    suffix = "effects-fs\\unit\\cdrnDestNonExtantFails\\dest"
#endif

cdrnOverwriteFails :: IO FilePath -> TestTree
cdrnOverwriteFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrnExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: This causes the expected error
  createDirectoryIfMissing False (destDir </> "src")

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
      <$> [ "src/a/",
            "src/empty"
          ]
  where
    desc = "Copy to extant dest/<target> fails"
#if !WINDOWS
    suffix = "effects-fs/unit/cdrnExtantFails/dest/src"
#else
    suffix = "effects-fs\\unit\\cdrnExtantFails\\dest\\src"
#endif

cdrnPartialFails :: IO FilePath -> TestTree
cdrnPartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrnPartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

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
  assertDirsDoNotExist [destDir </> "src"]
  where
    desc = "Partial failure rolls back changes"

cdrOverwriteTargetTests :: IO FilePath -> TestTree
cdrOverwriteTargetTests getTmpDir =
  testGroup
    "OverwriteTarget"
    [ cdrtFresh getTmpDir,
      cdrtDestNonExtantFails getTmpDir,
      cdrtOverwriteTargetSucceeds getTmpDir,
      cdrtOverwriteFileFails getTmpDir,
      cdrtPartialFails getTmpDir,
      cdrtOverwritePartialFails getTmpDir
    ]

cdrtFresh :: IO FilePath -> TestTree
cdrtFresh getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtFresh") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteTarget)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrtDestNonExtantFails :: IO FilePath -> TestTree
cdrtDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtDestNonExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig (overwriteConfig OverwriteTarget) srcDir destDir
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
#if !WINDOWS
    suffix = "effects-fs/unit/cdrtDestNonExtantFails/dest"
#else
    suffix = "effects-fs\\unit\\cdrtDestNonExtantFails\\dest"
#endif

cdrtOverwriteTargetSucceeds :: IO FilePath -> TestTree
cdrtOverwriteTargetSucceeds getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtOverwriteTargetSucceeds") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: test that dir already exists and succeeds
  createDirectoryIfMissing False (destDir </> "src")
  createDirectoryIfMissing False (destDir </> "src/test")
  writeFiles [(destDir </> "src/test/here", "cat")]

  -- copy files
  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteTarget)
    srcDir
    destDir

  assertSrcExists tmpDir
  assertFilesExist [destDir </> "src/test/here"]
  assertDestExists tmpDir
  where
    desc = "copy to extant dest/<target> succeeds"

cdrtOverwriteFileFails :: IO FilePath -> TestTree
cdrtOverwriteFileFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtOverwriteFileFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing True (destDir </> "src/a/b/c")

  -- NOTE: this line causes it to die
  writeFiles [(destDir </> "src/a/b/c/f1", "cat")]

  -- copy files
  result <-
    tryCS $
      PathWriter.copyDirectoryRecursiveConfig
        (overwriteConfig OverwriteTarget)
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
  assertFilesExist [destDir </> "src/a/b/c/f1"]
  where
    desc = "copy to extant dest/<target>/file fails"
#if !WINDOWS
    suffix = "effects-fs/unit/cdrtOverwriteFileFails/dest/src/a/b/c/f1"
#else
    suffix = "effects-fs\\unit\\cdrtOverwriteFileFails\\dest\\src\\a\\b\\c\\f1"
#endif

cdrtPartialFails :: IO FilePath -> TestTree
cdrtPartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtPartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteTarget)
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
  assertDirsDoNotExist [destDir </> "src"]
  where
    desc = "Partial failure rolls back changes"

cdrtOverwritePartialFails :: IO FilePath -> TestTree
cdrtOverwritePartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrtOverwritePartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: test overwriting
  createDirectoryIfMissing False (destDir </> "src")
  createDirectoryIfMissing False (destDir </> "src/test")
  writeFiles [(destDir </> "src/test/here", "cat")]

  -- copy files
  result <-
    tryCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteTarget)
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
      <$> [ "src/a/",
            "src/empty"
          ]

  -- assert original file exists after copy failure
  assertFilesExist [destDir </> "src/test/here"]
  where
    desc = "Partial failure with extant dest/<target> rolls back changes"

cdrOverwriteAllTests :: IO FilePath -> TestTree
cdrOverwriteAllTests getTmpDir =
  testGroup
    "OverwriteAll"
    [ cdraOverwriteFileSucceeds getTmpDir
    ]

cdraOverwriteFileSucceeds :: IO FilePath -> TestTree
cdraOverwriteFileSucceeds getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdraOverwriteFileSucceeds") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing True (destDir </> "src/a/b/c")

  -- NOTE: this line is what is tested
  writeFiles [(destDir </> "src/a/b/c/f1", "cat")]
  assertFileContents [(destDir </> "src/a/b/c/f1", "cat")]

  -- copy files
  PathWriter.copyDirectoryRecursiveConfig
    (overwriteConfig OverwriteAll)
    srcDir
    destDir

  assertSrcExists tmpDir
  -- check contents actually overwritten
  assertFileContents [(destDir </> "src/a/b/c/f1", "1")]
  assertDestExists tmpDir
  where
    desc = "Copy to extant dest/<target>/file succeeds"

-------------------------------------------------------------------------------
--                                  Setup                                    --
-------------------------------------------------------------------------------

setupSrc :: (HasCallStack) => Path -> IO Path
setupSrc baseDir = do
  let files = ["a/b/c/f1", "a/f2", "a/b/f3", "a/f4", "a/f5", "a/b/f5"]
      srcDir = baseDir </> "src"

  -- create directories and files
  createDirectoryIfMissing True (srcDir </> "a/b/c")
  createDirectoryIfMissing True (srcDir </> "empty/d")

  let baseFiles = zip files ["1", "2", "3", "4", "5", "6"]
      srcFiles = fmap (first (srcDir </>)) baseFiles

  writeFiles srcFiles
  assertSrcExists baseDir
  pure srcDir

writeFiles :: (HasCallStack) => [(Path, ByteString)] -> IO ()
writeFiles = traverse_ (uncurry writeBinaryFile)

overwriteConfig :: Overwrite -> CopyDirConfig
overwriteConfig ow = MkCopyDirConfig ow Nothing

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

assertSrcExists :: (HasCallStack) => Path -> IO ()
assertSrcExists baseDir = do
  let srcDir = baseDir </> "src"
  assertFilesExist $
    (srcDir </>)
      <$> [ "a/b/c/f1",
            "a/f2",
            "a/b/f3",
            "a/f4",
            "a/f5",
            "a/b/f5"
          ]
  assertDirsExist $
    (srcDir </>)
      <$> [ "a/b/c",
            "empty/d"
          ]

assertDestExists :: (HasCallStack) => Path -> IO ()
assertDestExists baseDir = do
  let destDir = baseDir </> "dest"
  assertFilesExist $
    (destDir </>)
      <$> [ "src/a/b/c/f1",
            "src/a/f2",
            "src/a/b/f3",
            "src/a/f4",
            "src/a/f5",
            "src/a/b/f5"
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ "src/a/b/c",
            "src/empty/d"
          ]

assertFilesExist :: (HasCallStack) => [Path] -> IO ()
assertFilesExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> p) exists

assertFileContents :: (HasCallStack) => [(Path, ByteString)] -> IO ()
assertFileContents = traverse_ $ \(p, expected) -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> p) exists
  actual <- readBinaryFile p
  expected @=? actual

assertDirsExist :: (HasCallStack) => [Path] -> IO ()
assertDirsExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory to exist: " <> p) exists

assertDirsDoNotExist :: (HasCallStack) => [Path] -> IO ()
assertDirsDoNotExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory not to exist: " <> p) (not exists)
