{-# LANGUAGE UndecidableInstances #-}

module Effects.Notify.Internal.Data.Note
  ( Note,
    mkNote,
    setBody,
    getBody,
    setSummary,
    getSummary,
    setTimeout,
    getTimeout,
    setTitle,
    getTitle,
    setUrgency,
    getUrgency,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Effects.Notify.Internal.Data.NotifyTimeout (NotifyTimeout)
import Effects.Notify.Internal.Data.NotifyUrgency (NotifyUrgency)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import GHC.Show (appPrec1)
import Optics.Core (A_Lens, LabelOptic (labelOptic), (^.))
import Optics.Core qualified as O

-- NOTE:
--
-- We want Note abstract as the implementation could change in ways that
-- should not impact the interface.

-- | Notification type.
--
-- @since 0.1
data Note
  = MkNoteInternal
      -- Body
      (Maybe Text)
      -- Summary
      Text
      -- Timeout
      (Maybe NotifyTimeout)
      -- Title
      (Maybe Text)
      -- NotifyUrgency
      (Maybe NotifyUrgency)
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (NFData)

instance Show Note where
  showsPrec i note =
    showParen
      (i >= appPrec1)
      ( showString "MkNote {body = "
          . showsPrec i (note ^. #body)
          . showString ", summary = "
          . showsPrec i (note ^. #summary)
          . showString ", timeout = "
          . showsPrec i (note ^. #timeout)
          . showString ", title = "
          . showsPrec i (note ^. #title)
          . showString ", urgency = "
          . showsPrec i (note ^. #urgency)
          . showString "}"
      )

-- | @since 0.1
instance HasField "body" Note (Maybe Text) where
  getField (MkNoteInternal a _ _ _ _) = a

-- | @since 0.1
instance HasField "summary" Note Text where
  getField (MkNoteInternal _ a _ _ _) = a

-- | @since 0.1
instance HasField "timeout" Note (Maybe NotifyTimeout) where
  getField (MkNoteInternal _ _ a _ _) = a

-- | @since 0.1
instance HasField "title" Note (Maybe Text) where
  getField (MkNoteInternal _ _ _ a _) = a

-- | @since 0.1
instance HasField "urgency" Note (Maybe NotifyUrgency) where
  getField (MkNoteInternal _ _ _ _ a) = a

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "body" k Note Note a b
  where
  labelOptic = O.lensVL $ \f (MkNoteInternal a1 a2 a3 a4 a5) ->
    fmap (\b -> MkNoteInternal b a2 a3 a4 a5) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "summary" k Note Note a b
  where
  labelOptic = O.lensVL $ \f (MkNoteInternal a1 a2 a3 a4 a5) ->
    fmap (\b -> MkNoteInternal a1 b a3 a4 a5) (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NotifyTimeout, b ~ Maybe NotifyTimeout) =>
  LabelOptic "timeout" k Note Note a b
  where
  labelOptic = O.lensVL $ \f (MkNoteInternal a1 a2 a3 a4 a5) ->
    fmap (\b -> MkNoteInternal a1 a2 b a4 a5) (f a3)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "title" k Note Note a b
  where
  labelOptic = O.lensVL $ \f (MkNoteInternal a1 a2 a3 a4 a5) ->
    fmap (\b -> MkNoteInternal a1 a2 a3 b a5) (f a4)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe NotifyUrgency, b ~ Maybe NotifyUrgency) =>
  LabelOptic "urgency" k Note Note a b
  where
  labelOptic = O.lensVL $ \f (MkNoteInternal a1 a2 a3 a4 a5) ->
    fmap (\b -> MkNoteInternal a1 a2 a3 a4 b) (f a5)
  {-# INLINE labelOptic #-}

-- | Makes a note from a text summary.
mkNote :: Text -> Note
mkNote summary = MkNoteInternal Nothing summary Nothing Nothing Nothing

-- | @since 0.1
setBody :: Maybe Text -> Note -> Note
setBody = O.set' #body

-- | @since 0.1
getBody :: Note -> Maybe Text
getBody = O.view #body

-- | @since 0.1
setSummary :: Text -> Note -> Note
setSummary = O.set' #summary

-- | @since 0.1
getSummary :: Note -> Text
getSummary = O.view #summary

-- | @since 0.1
setTimeout :: Maybe NotifyTimeout -> Note -> Note
setTimeout = O.set' #timeout

-- | @since 0.1
getTimeout :: Note -> Maybe NotifyTimeout
getTimeout = O.view #timeout

-- | @since 0.1
setTitle :: Maybe Text -> Note -> Note
setTitle = O.set' #title

-- | @since 0.1
getTitle :: Note -> Maybe Text
getTitle = O.view #title

-- | @since 0.1
setUrgency :: Maybe NotifyUrgency -> Note -> Note
setUrgency = O.set' #urgency

-- | @since 0.1
getUrgency :: Note -> Maybe NotifyUrgency
getUrgency = O.view #urgency
