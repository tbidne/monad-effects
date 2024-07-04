{-# LANGUAGE CPP #-}

module TestUtils
  ( -- * Text Utils
    displayExceptiont,
    showt,
    strip,

    -- * Asserts
    assertContainsMinLines,
    assertContains,
  )
where

import Control.Exception (Exception (displayException))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.HUnit (assertBool)

showt :: (Show a) => Maybe a -> Text
showt = T.pack . show

displayExceptiont :: (Exception e) => e -> Text
displayExceptiont = T.pack . displayException

assertContainsMinLines :: Int -> [Text] -> Text -> IO ()
assertContainsMinLines minExpectedLines expected txt = do
  assertBool actualLinesErrMsg (actualLines >= minExpectedLines)
  assertContains expected txt
  where
    -- actualLines = newlines + 1 (line before first newline)
    actualLines = 1 + T.count "\n" txt

    actualLinesErrMsg =
      mconcat
        [ "Expected at least ",
          show minExpectedLines,
          ", received ",
          show actualLines,
          ": ",
          T.unpack (formatErrOutput txt)
        ]

assertContains :: [Text] -> Text -> IO ()
assertContains expected txt = do
  for_ expected $ \e -> do
    let found = e `T.isInfixOf` txt
        errMsg =
          mconcat
            [ "Expected element: '",
              e,
              "'\nReceived:",
              formatErrOutput txt
            ]
    assertBool (T.unpack errMsg) found

formatErrOutput :: Text -> Text
formatErrOutput = (prefix <>) . T.replace "\n" prefix
  where
    prefix = "\n  "

-- NOTE: [Base 4.20+ Output Differences]
--
-- The output changes with base 4.20. The main change is that SomeException's
-- displayException now prints annotations by default. We don't _really_
-- care about the differences since anyone on 4.20+ should be using the
-- new annotations and not the Legacy module. Nevertheless, we still want
-- to assert that the basic functionality still works.
--
-- To do this, we use CPP to expect different output where necessary, and
-- provide helper functions below.

-- Strip whitespace. base 4.20's SomeException instance adds newlines.
strip :: String -> String
#if MIN_VERSION_base(4,20,0)
strip = T.unpack . T.strip . T.pack
#else
strip = id
#endif
