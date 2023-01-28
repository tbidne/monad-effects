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
    tryWithCS,
  )
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
  ( MonadPathWriter (..),
    PathDoesNotExistException,
    PathExistsException,
    createDirectoryIfMissing,
  )
import Effects.FileSystem.PathWriter qualified as PathWriter
import Effects.IORef (MonadIORef (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

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
    [ cdrNoOverwriteTests getTmpDir,
      cdrOverwriteTests getTmpDir
    ]

cdrNoOverwriteTests :: IO FilePath -> TestTree
cdrNoOverwriteTests getTmpDir =
  testGroup
    "Overwrite = False"
    [ cdrFresh getTmpDir,
      cdrDestNonExtantFails getTmpDir,
      cdrOverwriteFails getTmpDir,
      cdrPartialFails getTmpDir
    ]

cdrFresh :: IO FilePath -> TestTree
cdrFresh getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrFresh") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursive False srcDir destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrDestNonExtantFails :: IO FilePath -> TestTree
cdrDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrDestNonExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <- tryWithCS $ PathWriter.copyDirectoryRecursive False srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: PathDoesNotExistException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path does not exist:" `L.isPrefixOf` exText)
  assertBool exText ("monad-fs/cdrDestNonExtantFails/dest" `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"

cdrOverwriteFails :: IO FilePath -> TestTree
cdrOverwriteFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: This causes the expected error
  createDirectoryIfMissing False (destDir </> "src")

  -- copy files
  result <- tryWithCS $ PathWriter.copyDirectoryRecursive False srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: PathExistsException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path already exists:" `L.isPrefixOf` exText)
  assertBool exText ("monad-fs/cdrExtantFails/dest/src" `L.isSuffixOf` exText)

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

cdrPartialFails :: IO FilePath -> TestTree
cdrPartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdrPartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryWithCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursive False srcDir destDir
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

cdrOverwriteTests :: IO FilePath -> TestTree
cdrOverwriteTests getTmpDir =
  testGroup
    "Overwrite = True"
    [ cdroFresh getTmpDir,
      cdroDestNonExtantFails getTmpDir,
      cdroOverwriteSucceeds getTmpDir,
      cdroPartialFails getTmpDir,
      cdroOverwritePartialFails getTmpDir
    ]

cdroFresh :: IO FilePath -> TestTree
cdroFresh getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdroFresh") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  PathWriter.copyDirectoryRecursive True srcDir destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdroDestNonExtantFails :: IO FilePath -> TestTree
cdroDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdroDestNonExtantFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <- tryWithCS $ PathWriter.copyDirectoryRecursive True srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: PathDoesNotExistException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Path does not exist:" `L.isPrefixOf` exText)
  assertBool exText ("monad-fs/cdroDestNonExtantFails/dest" `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"

cdroOverwriteSucceeds :: IO FilePath -> TestTree
cdroOverwriteSucceeds getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdroOverwriteSucceeds") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: test that dir already exists and succeeds
  createDirectoryIfMissing False (destDir </> "src")
  createDirectoryIfMissing False (destDir </> "src/test")
  writeFiles [(destDir </> "src/test/here", "cat")]

  -- copy files
  PathWriter.copyDirectoryRecursive True srcDir destDir

  assertSrcExists tmpDir
  assertFilesExist [destDir </> "src/test/here"]
  assertDestExists tmpDir
  where
    desc = "Copies to extant dest/<target> succeeds"

cdroPartialFails :: IO FilePath -> TestTree
cdroPartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdroPartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- copy files
  result <-
    tryWithCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursive True srcDir destDir
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

cdroOverwritePartialFails :: IO FilePath -> TestTree
cdroOverwritePartialFails getTmpDir = testCase desc $ do
  tmpDir <- (</> "cdroOverwritePartialFails") <$> getTmpDir
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> "dest"

  createDirectoryIfMissing False destDir

  -- NOTE: test overwriting
  createDirectoryIfMissing False (destDir </> "src")
  createDirectoryIfMissing False (destDir </> "src/test")
  writeFiles [(destDir </> "src/test/here", "cat")]

  -- copy files
  result <-
    tryWithCS $
      runPartialIO $
        PathWriter.copyDirectoryRecursive True srcDir destDir
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

-------------------------------------------------------------------------------
--                                  Setup                                    --
-------------------------------------------------------------------------------

setupSrc :: HasCallStack => Path -> IO Path
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

writeFiles :: HasCallStack => [(Path, ByteString)] -> IO ()
writeFiles = traverse_ (uncurry writeBinaryFile)

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

{-deriving
  ( MonadTrans
  )
  via (ReaderT (IORef Word8))-}

-- type PartialIO = PartialT IO

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

assertSrcExists :: HasCallStack => Path -> IO ()
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

assertDestExists :: HasCallStack => Path -> IO ()
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

assertFilesExist :: HasCallStack => [Path] -> IO ()
assertFilesExist = traverse_ $ \p -> do
  exists <- doesFileExist p
  assertBool ("Expected file to exist: " <> p) exists

assertDirsExist :: HasCallStack => [Path] -> IO ()
assertDirsExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory to exist: " <> p) exists

assertDirsDoNotExist :: HasCallStack => [Path] -> IO ()
assertDirsDoNotExist = traverse_ $ \p -> do
  exists <- doesDirectoryExist p
  assertBool ("Expected directory not to exist: " <> p) (not exists)
