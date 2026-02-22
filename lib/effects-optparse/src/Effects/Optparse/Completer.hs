-- | Provides additional 'Completer's.
--
-- @since 0.1
module Effects.Optparse.Completer
  ( -- * Bash completion

    -- ** Aggregate completers
    compgenCwdPathsCompleter,
    compgenCwdDirsCompleter,
    compgenCwdPathsSuffixCompleter,

    -- ** Compen
    bashCompleterQuiet,

    -- ** Pure haskell
    cwdPathsCompleter,
    cwdDirsCompleter,
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

-- | Like 'compgenCwdPathsCompleter' but returns directories only.
--
-- @since 0.1
compgenCwdDirsCompleter :: Completer
compgenCwdDirsCompleter = bashCompleterQuiet "directory" <> cwdDirsCompleter

-- | 'compgenCwdPathsCompleter' that filters on the given suffix.
--
-- @since -.1
compgenCwdPathsSuffixCompleter :: String -> Completer
compgenCwdPathsSuffixCompleter sfx =
  bashCompleterQuiet compgenFilter
    <> cwdPathsCompleterFilter (PathFilterStr strFilter)
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
cwdPathsCompleter = cwdPathsCompleterFilter (PathFilterStr $ const True)

-- | Like 'cwdPathsCompleter' but returns directories only.
--
-- @since 0.1
cwdDirsCompleter :: Completer
cwdDirsCompleter = cwdPathsCompleterFilter $ PathFilterOsPIO Dir.doesDirectoryExist

-- | Filters paths.
data PathFilter
  = -- | Simple predicate on the path's lenient decode to 'String'.
    --
    -- @since 0.1
    PathFilterStr (String -> Bool)
  | -- | Simple predicate on the path.
    --
    -- @since 0.1
    PathfilterOsP (OsPath -> Bool)
  | -- | Effectful predicate on the path's lenient decode to 'String'.
    --
    -- @since 0.1
    PathFilterStrIO (String -> IO Bool)
  | -- | Effectful predicate on the path.
    --
    -- @since 0.1
    PathFilterOsPIO (OsPath -> IO Bool)

-- | 'cwdPathsCompleter' that runs an additional filter on paths.
--
-- @since 0.1
cwdPathsCompleterFilter :: PathFilter -> Completer
cwdPathsCompleterFilter pfilter = OAC.mkCompleter $ \word -> do
  eFiles <- tryIO $ Dir.getCurrentDirectory >>= Dir.listDirectory

  let files = fromRight [] eFiles
      myFoldr :: forall a. a -> (String -> OsPath -> a -> a) -> a
      myFoldr initial p = foldr (p word) initial files

  case pfilter of
    PathFilterStr predFn -> pure $ myFoldr [] $ \wd p acc ->
      let pStr = OsPath.decodeLenient p
          matchesPat = wd `L.isPrefixOf` pStr
       in if matchesPat && predFn pStr
            then pStr : acc
            else acc
    PathfilterOsP predFn -> pure $ myFoldr [] $ \wd p acc ->
      let pStr = OsPath.decodeLenient p
          matchesPat = wd `L.isPrefixOf` pStr
       in if matchesPat && predFn p
            then pStr : acc
            else acc
    PathFilterStrIO predFn -> myFoldr (pure []) $ \wd p acc -> do
      let pStr = OsPath.decodeLenient p
          matchesPat = wd `L.isPrefixOf` pStr

      if matchesPat
        then do
          c <- predFn pStr
          if c
            then (pStr :) <$> acc
            else acc
        else acc
    PathFilterOsPIO predFn -> myFoldr (pure []) $ \wd p acc -> do
      let pStr = OsPath.decodeLenient p
          matchesPat = wd `L.isPrefixOf` pStr

      if matchesPat
        then do
          c <- predFn p
          if c
            then (pStr :) <$> acc
            else acc
        else acc

tryIO :: (MonadCatch m) => m a -> m (Either IOException a)
tryIO = C.try
