{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Misc (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Effects.Exception (MonadCatch, catchAny, displayException, tryAny)
import Effects.FileSystem.PathWriter qualified as WDir
import Effects.FileSystem.Utils (OsPath, osp, (</>))
import Effects.FileSystem.Utils qualified as Utils
import Hedgehog (MonadGen, MonadTest, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "Misc"
    [ readWriteRoundtrip getTmpDir,
      matchesBytestring getTmpDir
    ]

-- | Tests that write/read is a roundtrip for valid paths.
readWriteRoundtrip :: IO OsPath -> TestTree
readWriteRoundtrip getTmpDir = testPropertyNamed desc "readWriteRoundtrip" $ do
  H.property $ do
    tmpDir <- (</> [osp|readWriteRoundtrip|]) <$> liftIO getTmpDir
    liftIO $ WDir.createDirectoryIfMissing True tmpDir
    H.annotateShow tmpDir

    fp <- H.forAll genGoodFilePath
    contents <- H.forAll genFileContents

    os <- hcatch (Utils.encodeFpToValidOsThrowM fp)

    let osPath = tmpDir </> os

    H.annotateShow osPath
    H.annotateShow contents

    hcatch (Utils.writeBinaryFileIO osPath contents)
    resultContents <- hcatch (Utils.readBinaryFileIO osPath)

    contents === resultContents
  where
    desc = "(read . write) round trips"

-- | Tests that write/read success for ByteString's API implies success for
-- Effect's API. This is essentially a regression test for our new OsPath
-- API.
matchesBytestring :: IO OsPath -> TestTree
matchesBytestring getTmpDir = testPropertyNamed desc "matchesBytestring" $ do
  H.property $ do
    tmpDir <- (</> [osp|matchesBytestring|]) <$> liftIO getTmpDir
    liftIO $ WDir.createDirectoryIfMissing True tmpDir
    H.annotateShow tmpDir

    fp <- H.forAll genAnyFilePath
    contents <- H.forAll genFileContents

    case Utils.encodeFpToValidOs fp of
      Left _ -> H.label "Encoding FilePath failed"
      Right os -> do
        tmpDir' <- Utils.decodeOsToFpThrowM tmpDir
        let filePath = tmpDir' `Utils.combineFilePaths` fp
            osPath = tmpDir </> os

        H.annotateShow osPath
        H.annotateShow contents

        -- The randomly generated path cannot be assumed to be valid, hence we
        -- must check for failure. This allows us to check a larger domain than
        -- matchesBytestring, at the cost of having to skip failures.
        eBsResult <- tryAny $ liftIO $ do
          BS.writeFile filePath contents
          BS.readFile filePath

        H.annotate "ByteString succeeded"

        case eBsResult of
          Left _ -> H.label "ByteString write failed"
          Right bsResultContents -> do
            H.label "ByteString write succeeded"
            hcatch (Utils.writeBinaryFileIO osPath contents)
            resultContents <- hcatch (Utils.readBinaryFileIO osPath)

            bsResultContents === resultContents
            contents === resultContents
  where
    desc = "ByteString.write/read succeeds => Effects.write/read succeeds"

hcatch :: (MonadCatch m, MonadIO m, MonadTest m) => IO a -> m a
hcatch m =
  liftIO m
    `catchAny` \ex -> do
      H.annotate $ displayException ex
      H.failure

genFileContents :: (MonadGen m) => m ByteString
genFileContents = Gen.utf8 range (Gen.filterT (/= '\0') Gen.unicode)
  where
    range = Range.linear 1 20

-- | Generates a random unicode string without restrictions. The result
-- is __not__ guaranteed to be a valid path on any platform.
genAnyFilePath :: (MonadGen m) => m FilePath
genAnyFilePath =
  Gen.string range Gen.unicode
  where
    range = Range.linear 1 20

-- | Generates a FilePath, excluding characters known to be invalid on each
-- platform. The resulting FilePath should encode successfully as a valid
-- OsPath.
genGoodFilePath :: (MonadGen m) => m FilePath
genGoodFilePath =
  Gen.string range genPathChar
  where
    range = Range.linear 1 20

genPathChar :: (MonadGen m) => m Char
genPathChar = Gen.filterT filterFn (charMapper <$> genChar)
  where
    filterFn c = isGoodChar c && Ch.isAlphaNum c

genChar :: (MonadGen m) => m Char
isGoodChar :: Char -> Bool
badChars :: HashSet Char
charMapper :: Char -> Char

#if OSX
-- This is hedgehog's built-in Gen.unicode restricted to plane 0
-- (Basic Multilingual Plane). For reasons I do not understand, osx on CI
-- often chokes on code points outside of this range even when properly
-- encoded as UTF-8.
--
-- This seems to be backed up by osx's behavior e.g. if you try to create a
-- a file with the filename "0x F0 B1 8D 90", you will receive the error
-- 'illegal byte sequence'. This is the UTF-8 encoding for the
-- '\201552' <-> 0x31350 code point, for the record.
--
-- Curiously, many of the 'illegal byte sequences' contain 0xF0 as the lead
-- byte in some pair e.g. 0xF0C2. This is by no means exhaustive, however.
-- The notion of "overlong sequences" seems possibly relevant, though if the
-- sequence is in fact illegal, then:
--
--    1. Why is it produced by OsPath's encodeToUtf and Text's encodeUtf8?
--    2. Why is it only osx that struggles and not linux?
--
-- In any case, we restrict osx to AlphaNum Plane 0, as these seem fine.
--
-- https://en.wikipedia.org/wiki/Plane_(Unicode)
-- https://en.wikipedia.org/wiki/UTF-8#Overlong_encodings
genChar =
  let
    -- s1 + s2 := Plane 0
    s1 =
      (55296, Gen.enum '\0' '\55295')
    s2 =
      (8190, Gen.enum '\57344' '\65533')
  in
    Gen.frequency [s1, s2]

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      ':'
    ]

charMapper = Ch.toLower
#elif WINDOWS
genChar = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

-- https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file#file-and-directory-names
badChars =
  Set.fromList
    [ '/',
      '\\',
      '<',
      '>',
      ':',
      '"',
      '|',
      '?',
      '*',
      '0',
      '.',
      ' '
    ]

-- windows paths are case-insensitive by default, so let's just take
-- lower-case paths :-(
charMapper = Ch.toLower
#else
genChar = Gen.unicode

isGoodChar c = (not . Ch.isControl) c && not (Set.member c badChars)

badChars =
  Set.fromList
    [ '/',
      '.',
      '*'
    ]

charMapper = id
#endif
