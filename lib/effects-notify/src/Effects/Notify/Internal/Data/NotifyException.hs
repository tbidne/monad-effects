{-# LANGUAGE UndecidableInstances #-}

module Effects.Notify.Internal.Data.NotifyException
  ( NotifyException (..),
  )
where

import Control.Exception
  ( Exception (displayException),
    SomeException,
  )
import Data.Text qualified as T
import Data.Text.Display (display)
import Effects.Notify.Internal.Data.Note (Note)
import Effects.Notify.Internal.Data.NotifyEnv (NotifyEnv)
import Optics.Core (A_Lens, LabelOptic (labelOptic), (^.))
import Optics.Core qualified as O

-- | Exceptions thrown when sending notifications. The list of non-fatal
-- exceptions includes:
--
-- - DBus: Non-fatal client errors.
--
-- All others are considered fatal.
--
-- @since 0.1
data NotifyException = MkNotifyException
  { -- | Underlying exception.
    exception :: SomeException,
    -- | Is fatal?
    fatal :: Bool,
    -- | Note we attempted to send.
    note :: Note,
    -- | Notification env.
    notifyEnv :: NotifyEnv
  }

-- | @since 0.1
deriving stock instance Show NotifyException

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ SomeException, b ~ SomeException) =>
  LabelOptic "exception" k NotifyException NotifyException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyException a1 a2 a3 a4) ->
    fmap (\b -> MkNotifyException b a2 a3 a4) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "fatal" k NotifyException NotifyException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyException a1 a2 a3 a4) ->
    fmap (\b -> MkNotifyException a1 b a3 a4) (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Note, b ~ Note) =>
  LabelOptic "note" k NotifyException NotifyException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyException a1 a2 a3 a4) ->
    fmap (\b -> MkNotifyException a1 a2 b a4) (f a3)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ NotifyEnv, b ~ NotifyEnv) =>
  LabelOptic "notifyEnv" k NotifyException NotifyException a b
  where
  labelOptic = O.lensVL $ \f (MkNotifyException a1 a2 a3 a4) ->
    fmap (\b -> MkNotifyException a1 a2 a3 b) (f a4)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Exception NotifyException where
  displayException ex =
    T.unpack $
      mconcat
        [ fatalStr,
          "exception sending notification with env '",
          display (ex ^. #notifyEnv),
          "' and note '",
          T.pack $ show (ex ^. #note),
          "': ",
          T.pack $ displayException (ex ^. #exception)
        ]
    where
      fatalStr = case ex ^. #fatal of
        False -> "Non-fatal "
        True -> "Fatal "
