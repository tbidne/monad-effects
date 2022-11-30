-- | Main module.
--
-- @since 0.1
module Main (main) where

import Lib qualified

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = putStrLn Lib.hello
