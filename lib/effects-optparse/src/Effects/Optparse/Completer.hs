-- | Provides additional 'Completer's.
--
-- @since 0.1
module Effects.Optparse.Completer
  ( -- * Bash completion

    -- ** Aggregate completers
    compgenCwdPathsCompleter,
    compgenCwdPathsSuffixCompleter,

    -- ** Compen
    bashCompleterQuiet,

    -- ** Pure haskell
    cwdPathsCompleter,
    cwdPathsCompleterFilter,

    -- * Misc
    OAC.requote,
  )
where

import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Catch qualified as C
import Data.Either (fromRight)
import Data.List qualified as L
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as OsPath
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Options.Applicative.Builder.Completer (Completer)
import Options.Applicative.Builder.Completer qualified as OAC
import System.Directory.OsPath qualified as Dir
import System.Process qualified as P

-- | Completer for cwd paths that first tries compgen via 'bashCompleterQuiet',
-- then falls back to 'cwdPathsCompleter'.
--
-- @since 0.1
compgenCwdPathsCompleter :: Completer
compgenCwdPathsCompleter = bashCompleterQuiet "file" <> cwdPathsCompleter

-- | 'compgenCwdPathsCompleter' that filters on the given suffix.
--
-- @since -.1
compgenCwdPathsSuffixCompleter :: String -> Completer
compgenCwdPathsSuffixCompleter sfx =
  bashCompleterQuiet compgenFilter
    <> cwdPathsCompleterFilter strFilter
  where
    compgenFilter =
      mconcat
        [ "file -X '!*",
          sfx,
          "'"
        ]
    strFilter = L.isSuffixOf sfx

-- | Like 'OAC.bashCompleter', except any compgen stderrs are swallowed.
-- This can be nicer when completion errors make the output messy.
--
-- @since 0.1
bashCompleterQuiet :: String -> Completer
bashCompleterQuiet action = OAC.mkCompleter $ \word -> do
  let cmd = L.unwords ["compgen", "-A", action, "--", OAC.requote word]
  (ec, out, _err) <- P.readCreateProcessWithExitCode (P.shell cmd) ""
  pure $ case ec of
    ExitFailure _ -> []
    ExitSuccess -> L.lines out

-- | Completer based on paths in the current directory. Does not require
-- external programs like compgen.
--
-- @since 0.1
cwdPathsCompleter :: Completer
cwdPathsCompleter = cwdPathsCompleterFilter (const True)

-- | 'cwdPathsCompleter' that runs an additional filter.
--
-- @since 0.1
cwdPathsCompleterFilter :: (String -> Bool) -> Completer
cwdPathsCompleterFilter predFn = OAC.mkCompleter $ \word -> do
  eFiles <- tryIO $ Dir.getCurrentDirectory >>= Dir.listDirectory

  let files = fromRight [] eFiles

  pure $ foldr (go word) [] files
  where
    go :: String -> OsPath -> [String] -> [String]
    go word p acc = do
      let pStr = OsPath.decodeLenient p
          matchesPat = word `L.isPrefixOf` pStr

      if matchesPat && predFn pStr
        then pStr : acc
        else acc

tryIO :: (MonadCatch m) => m a -> m (Either IOException a)
tryIO = C.try
