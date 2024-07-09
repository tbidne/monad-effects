module Utils (tests) where

import Control.Monad (void)
import Effects.FileSystem.Utils (OsPath)
import Effects.FileSystem.Utils qualified as Utils
import Hedgehog (Gen, evalNF, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath qualified as OsPath
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests =
  testGroup
    "Utils"
    [ testEncodeLenientTotal,
      testEncodeValidLenientTotal,
      testDecodeLenientTotal
    ]

testEncodeLenientTotal :: TestTree
testEncodeLenientTotal = testPropertyNamed desc "testEncodeLenientTotal" $ do
  H.property $ do
    fp <- forAll genFilePath

    let encoded = Utils.encodeFpToOsLenient fp

    void $ evalNF encoded
  where
    desc = "encodeFpToOsLenient is total"

testEncodeValidLenientTotal :: TestTree
testEncodeValidLenientTotal = testPropertyNamed desc "testEncodeValidLenientTotal" $ do
  H.property $ do
    fp <- forAll genFilePath

    let encoded = Utils.encodeFpToValidOsLenient fp

    void $ evalNF encoded
  where
    desc = "encodeFpToValidOsLenient is total"

testDecodeLenientTotal :: TestTree
testDecodeLenientTotal = testPropertyNamed desc "testDecodeLenientTotal" $ do
  H.property $ do
    osPath <- forAll genSketchyOsPath

    let decoded = Utils.decodeOsToFpLenient osPath

    void $ evalNF decoded
  where
    desc = "decodeOsToFpLenient is total"

-- | The idea is, generate a possibly invalid OsPath s.t. generation is total
-- but decoding might fail. Turns out this works i.e. if we replace
-- decodeOsToFpLenient with unsafeDecodeOsToFp, then the test fails
-- (which is what we want, since we want to test that lenient is actually
-- doing something).
genSketchyOsPath :: Gen OsPath
genSketchyOsPath = do
  str <- genFilePath
  let osCharList = OsPath.unsafeFromChar <$> str
  pure $ OsPath.pack osCharList

genFilePath :: Gen FilePath
genFilePath = Gen.string (Range.linearFrom 0 0 50) Gen.unicodeAll
