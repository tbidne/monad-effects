-- | Provides namespaced logging functionality on top of 'MonadLogger'.
--
-- @since 0.1
module Effects.Logger.Namespace
  ( -- * Effect
    Utils.Namespace (..),
    addNamespace,

    -- * Constraint aliases
    HasNamespace,
    MonadLoggerNS,

    -- ** Logging functions

    -- * Formatting
    LogFormatter (..),
    Utils.defaultLogFormatter,
    LocStrategy (..),
    formatLog,

    -- * LogStr
    Utils.logStrToBs,
    Utils.logStrToText,

    -- * Optics

    -- ** LocStrategy
    _LocPartial,
    _LocStable,
    _LocNone,
  )
where

import Control.Monad.Logger
  ( LogLevel,
    LogStr,
    MonadLogger,
    ToLogStr,
  )
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.Kind (Constraint, Type)
import Data.Sequence (Seq ((:|>)))
import Data.Text (Text)
import Effects.Concurrent.Thread (MonadThread)
import Effects.Logger.Utils
  ( LocStrategy (LocNone, LocPartial, LocStable),
    LogFormatter (MkLogFormatter, locStrategy, newline, threadLabel, timezone),
    Namespace,
  )
import Effects.Logger.Utils qualified as Utils
import Effects.Time (MonadTime)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (Loc)
import Optics.Core
  ( A_Getter,
    A_Setter,
    Is,
    LabelOptic',
    NoIx,
    Optic',
    Prism',
    Setter',
    castOptic,
    over',
    prism,
    view,
    (%),
  )

-- | Alias for constraints required to use namespaces.
type HasNamespace :: (Type -> Type) -> Type -> Type -> Constraint
type HasNamespace m env k =
  ( Is k A_Getter,
    Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  )

-- | Alias for HasNamespace and MonadLogger constraint.
type MonadLoggerNS :: (Type -> Type) -> Type -> Type -> Constraint
type MonadLoggerNS m env k = (HasNamespace m env k, MonadLogger m)

-- | Adds to the namespace.
--
-- @since 0.1
addNamespace ::
  forall m env a k.
  ( Is k A_Setter,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m
  ) =>
  Text ->
  m a ->
  m a
addNamespace txt = local (over' (castSet #namespace % #unNamespace) (:|> txt))
  where
    -- See https://github.com/well-typed/optics/issues/368 for why this is
    -- necessary.
    castSet :: Optic' k NoIx env Namespace -> Setter' env Namespace
    castSet = castOptic @A_Setter @k @_ @env @env @Namespace @Namespace
{-# INLINEABLE addNamespace #-}

-- | @since 0.1
_LocPartial :: Prism' LocStrategy Loc
_LocPartial =
  prism
    LocPartial
    ( \case
        LocPartial loc -> Right loc
        other -> Left other
    )
{-# INLINE _LocPartial #-}

-- | @since 0.1
_LocStable :: Prism' LocStrategy Loc
_LocStable =
  prism
    LocStable
    ( \case
        LocStable loc -> Right loc
        other -> Left other
    )
{-# INLINE _LocStable #-}

-- | @since 0.1
_LocNone :: Prism' LocStrategy ()
_LocNone =
  prism
    (const LocNone)
    ( \case
        LocNone -> Right ()
        other -> Left other
    )
{-# INLINE _LocNone #-}

-- | Produces a formatted 'LogStr'.
--
-- __Example__
--
-- @
-- -- [timestamp][thread_label][namespace][code_loc][level] msg
-- [2022-02-08 10:20:05][thread-label][one.two][filename:1:2][Warn] msg
-- @
--
-- @since 0.1
formatLog ::
  forall m env msg k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "namespace" k env Namespace,
    MonadReader env m,
    MonadThread m,
    MonadTime m,
    ToLogStr msg
  ) =>
  -- | Formatter to use.
  LogFormatter ->
  -- | The level in which to log.
  LogLevel ->
  -- | Message.
  msg ->
  -- | Formatted LogStr.
  m LogStr
formatLog formatter lvl msg = do
  ns <- asks (view #namespace)
  Utils.formatLog (Just ns) formatter lvl msg
{-# INLINEABLE formatLog #-}
