-- | Provides the 'MonadOptparse' typeclass.
--
-- @since 0.1
module Effects.MonadOptparse
  ( -- * Effect
    MonadOptparse (..),
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Effects.MonadCallStack (addCallStack)
import GHC.Stack (HasCallStack)
import Options.Applicative (ParserInfo, ParserPrefs, ParserResult)
import Options.Applicative qualified as OA
import Prelude hiding (getChar, getLine, print, putStr, putStrLn)

-- | Effects for
-- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative.html).
--
-- The vast majority of optparse-applicative is not re-exported, as most of
-- the interface is pure. This is merely the functions needed to run a parser.
--
-- @since 0.1
class Monad m => MonadOptparse m where
  -- | Lifted 'OA.execParser'.
  --
  -- @since 0.1
  execParser :: HasCallStack => ParserInfo a -> m a

  -- | Lifted 'OA.customExecParser'.
  --
  -- @since 0.1
  customExecParser :: HasCallStack => ParserPrefs -> ParserInfo a -> m a

  -- | Lifted 'OA.handleParseResult'.
  --
  -- @since 0.1
  handleParseResult :: HasCallStack => ParserResult a -> m a

-- | @since 0.1
instance MonadOptparse IO where
  execParser = addCallStack . OA.execParser
  {-# INLINEABLE execParser #-}
  customExecParser p = addCallStack . OA.customExecParser p
  {-# INLINEABLE customExecParser #-}
  handleParseResult = addCallStack . OA.handleParseResult
  {-# INLINEABLE handleParseResult #-}

-- | @since 0.1
instance MonadOptparse m => MonadOptparse (ReaderT env m) where
  execParser = lift . execParser
  {-# INLINEABLE execParser #-}
  customExecParser p = lift . customExecParser p
  {-# INLINEABLE customExecParser #-}
  handleParseResult = lift . handleParseResult
  {-# INLINEABLE handleParseResult #-}
