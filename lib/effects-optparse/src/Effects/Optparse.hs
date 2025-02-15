-- | Provides the 'MonadOptparse' typeclass.
--
-- @since 0.1
module Effects.Optparse
  ( -- * Effect
    MonadOptparse (..),

    -- * Misc
    OsPath,
    osPath,
    validOsPath,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import FileSystem.OsPath (OsPath)
import FileSystem.OsPath qualified as FS.OsPath
import GHC.Stack (HasCallStack)
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult, ReadM)
import Options.Applicative qualified as OA

-- | Effects for
-- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative.html).
--
-- The vast majority of optparse-applicative is not re-exported, as most of
-- the interface is pure. This is merely the functions needed to run a parser.
--
-- @since 0.1
class (Monad m) => MonadOptparse m where
  -- | Lifted 'OA.execParser'.
  --
  -- @since 0.1
  execParser :: (HasCallStack) => ParserInfo a -> m a

  -- | Lifted 'OA.customExecParser'.
  --
  -- @since 0.1
  customExecParser :: (HasCallStack) => ParserPrefs -> ParserInfo a -> m a

  -- | Lifted 'OA.handleParseResult'.
  --
  -- @since 0.1
  handleParseResult :: (HasCallStack) => ParserResult a -> m a

-- | @since 0.1
instance MonadOptparse IO where
  execParser = OA.execParser
  {-# INLINEABLE execParser #-}
  customExecParser = OA.customExecParser
  {-# INLINEABLE customExecParser #-}
  handleParseResult = OA.handleParseResult
  {-# INLINEABLE handleParseResult #-}

-- | @since 0.1
instance (MonadOptparse m) => MonadOptparse (ReaderT env m) where
  execParser = lift . execParser
  {-# INLINEABLE execParser #-}
  customExecParser p = lift . customExecParser p
  {-# INLINEABLE customExecParser #-}
  handleParseResult = lift . handleParseResult
  {-# INLINEABLE handleParseResult #-}

-- | 'OsPath' 'OA.Option' reader.
--
-- @since 0.1
osPath :: ReadM OsPath
osPath = OA.str >>= FS.OsPath.encodeFail

-- | 'OsPath' 'OA.Option' reader. This includes validation i.e. fails if the
-- path is considered invalid on the given platform.
--
-- @since 0.1
validOsPath :: ReadM OsPath
validOsPath = OA.str >>= FS.OsPath.encodeValidFail
