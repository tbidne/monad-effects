{-# LANGUAGE CPP #-}

module Misc (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.Char qualified as Ch
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Effects.Exception (MonadCatch, catchAny, displayException)
import Effects.FileSystem.Utils (OsPath, (</>))
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
    [ readWriteRoundtrip getTmpDir
    ]

readWriteRoundtrip :: IO OsPath -> TestTree
readWriteRoundtrip getTmpDir = testPropertyNamed desc "readWriteRoundtrip" $ do
  H.property $ do
    tmpDir <- liftIO getTmpDir

    fp <- H.forAll genFilePath
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

genFilePath :: (MonadGen m) => m String
genFilePath =
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
    -- s1 + s1 := Plane 0
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
