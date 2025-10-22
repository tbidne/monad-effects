module Effects.Optparse.Completer
  ( -- * Type
    Completer,

    -- * Simple completers
    mkCompleter,
    listMCompleter,
    listCompleter,

    -- * Bash completion

    -- ** Aggregate completers
    compgenCwdPathsCompleter,
    compgenCwdPathsSuffixCompleter,

    -- ** Compen
    bashCompleter,
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
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.FileSystem.PathReader qualified as PR
import Effects.Optparse (OsPath)
import Effects.System.Process (MonadProcess)
import Effects.System.Process qualified as P
import FileSystem.OsPath qualified as OsPath
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Options.Applicative.Builder.Completer qualified as OAC

-- | A shell complete function runs in some m.
--
-- @since 0.1
newtype Completer m = MkCompleter (String -> m [String])

-- | @since 0.1
instance (Applicative m) => Semigroup (Completer m) where
  MkCompleter c1 <> MkCompleter c2 =
    MkCompleter $ \s -> (++) <$> c1 s <*> c2 s

-- | @since 0.1
instance (Applicative m) => Monoid (Completer m) where
  mempty = MkCompleter $ \_ -> pure []

-- | Smart constructor for 'Completer'.
--
-- @since 0.1
mkCompleter :: (String -> m [String]) -> Completer m
mkCompleter = MkCompleter

-- | Creates a completer from an M-action producing a list of strings.
--
-- @since 0.1
listMCompleter :: (Functor m) => m [String] -> Completer m
listMCompleter ss = MkCompleter $ \s ->
  filter (L.isPrefixOf s) <$> ss

-- | Creates a completer from a constant list of strings.
--
-- @since 0.1
listCompleter :: (Applicative m) => [String] -> Completer m
listCompleter = listMCompleter . pure

-- | Completer for cwd paths that first tries compgen via 'bashCompleterQuiet',
-- then falls back to 'cwdPathsCompleter'.
--
-- @since 0.1
compgenCwdPathsCompleter ::
  ( MonadCatch m,
    MonadPathReader m,
    MonadProcess m
  ) =>
  Completer m
compgenCwdPathsCompleter = bashCompleterQuiet "file" <> cwdPathsCompleter

-- | 'compgenCwdPathsCompleter' that filters on the given suffix.
--
-- @since -.1
compgenCwdPathsSuffixCompleter ::
  ( MonadCatch m,
    MonadPathReader m,
    MonadProcess m
  ) =>
  String ->
  Completer m
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

-- | Like 'bashCompleter', except any compgen stderrs are swallowed.
-- This can be nicer when completion errors make the output messy.
--
-- @since 0.1
bashCompleterQuiet :: (MonadProcess m) => String -> Completer m
bashCompleterQuiet action = MkCompleter $ \word -> do
  let cmd = L.unwords ["compgen", "-A", action, "--", OAC.requote word]
  (ec, out, _err) <- P.readCreateProcessWithExitCode (P.shell cmd) ""
  pure $ case ec of
    ExitFailure _ -> []
    ExitSuccess -> L.lines out

-- | Runs compgen with the given action e.g. "file" or "directory".
--
-- @since 0.1
bashCompleter :: (MonadCatch m, MonadProcess m) => String -> Completer m
bashCompleter action = MkCompleter $ \word -> do
  let cmd = L.unwords ["compgen", "-A", action, "--", OAC.requote word]
  eResult <- tryIO $ P.readCreateProcess (P.shell cmd) ""
  pure $ case eResult of
    Left _ -> []
    Right out -> L.lines out

-- | Completer based on paths in the current directory. Does not require
-- external programs like compgen.
--
-- @since 0.1
cwdPathsCompleter ::
  ( MonadCatch m,
    MonadPathReader m
  ) =>
  Completer m
cwdPathsCompleter = cwdPathsCompleterFilter (const True)

-- | 'cwdPathsCompleter' that runs an additional filter.
--
-- @since 0.1
cwdPathsCompleterFilter ::
  forall m.
  ( MonadCatch m,
    MonadPathReader m
  ) =>
  -- | Filter function.
  (String -> Bool) ->
  Completer m
cwdPathsCompleterFilter predFn = mkCompleter $ \word -> do
  eFiles <- C.try @_ @IOException $ do
    cwd <- PR.getCurrentDirectory
    PR.listDirectory cwd

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
