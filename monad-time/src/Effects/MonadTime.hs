{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module Effects.MonadTime
  ( -- * Class
    MonadTime (..),

    -- * Timing
    TimeSpec (..),
    toTimeSpec,
    fromTimeSpec,
    withTiming,
    withTiming_,

    -- * Formatting
    formatLocalTime,
    formatZonedTime,

    -- * Parsing
    parseLocalTime,
    parseLocalTimeCallStack,
    parseZonedTime,
    parseZonedTimeCallStack,

    -- * Reexports
    LocalTime (..),
    ZonedTime (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime (LocalTime, localDay, localTimeOfDay),
    ZonedTime (ZonedTime, zonedTimeToLocalTime, zonedTimeZone),
  )
import Data.Time.LocalTime qualified as Local
import Effects.MonadCallStack (MonadCallStack, checkpointCallStack)
import GHC.Clock qualified as C
#if MIN_VERSION_base(4,17,0)
import GHC.Float (properFractionDouble)
#endif
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Structure for holding time data.
--
-- @since 0.1
data TimeSpec = MkTimeSpec
  { -- | Seconds.
    --
    -- @since 0.1
    sec :: {-# UNPACK #-} !Natural,
    -- | Nanoseconds.
    --
    -- @since 0.1
    nsec :: {-# UNPACK #-} !Natural
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''TimeSpec

-- | Converts a 'Double' to a 'TimeSpec'.
--
-- @since 0.1
toTimeSpec :: Double -> TimeSpec
toTimeSpec d =
  MkTimeSpec
    { sec = seconds,
      nsec = nanoseconds
    }
  where
    (seconds, remainder) = properFractionDouble d
    nanoseconds = floor $ remainder * 1_000_000_000

-- | Converts a 'TimeSpec' to a 'Double'.
--
-- @since 0.1
fromTimeSpec :: TimeSpec -> Double
fromTimeSpec (MkTimeSpec s n) =
  fromIntegral s + (fromIntegral n / 1_000_000_000)

-- | Time effect.
--
-- @since 0.1
class Monad m => MonadTime m where
  -- | Returns the local system time.
  --
  -- @since 0.1
  getSystemTime :: HasCallStack => m LocalTime

  -- | Returns the zoned system time
  --
  -- @since 0.1
  getSystemZonedTime :: HasCallStack => m ZonedTime

  -- | Return monotonic time in seconds, since some unspecified starting
  -- point.
  --
  -- @since 0.1
  getMonotonicTime :: HasCallStack => m Double

-- | @since 0.1
instance MonadTime IO where
  getSystemTime =
    checkpointCallStack
      (Local.zonedTimeToLocalTime <$> Local.getZonedTime)
  getSystemZonedTime = checkpointCallStack Local.getZonedTime
  getMonotonicTime = checkpointCallStack C.getMonotonicTime

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getSystemZonedTime = lift getSystemZonedTime
  getMonotonicTime = lift getMonotonicTime

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming ::
  ( HasCallStack,
    MonadTime m
  ) =>
  m a ->
  m (TimeSpec, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (toTimeSpec (end - start), res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ ::
  ( HasCallStack,
    MonadTime m
  ) =>
  m a ->
  m TimeSpec
withTiming_ = fmap fst . withTiming

-- | Formats the 'ZonedTime' to @YYYY-MM-DD HH:MM:SS Z@.
--
-- @since 0.1
formatZonedTime :: ZonedTime -> String
formatZonedTime = Format.formatTime Format.defaultTimeLocale zonedTimeFormat

-- | Formats the 'LocalTime' to @YYYY-MM-DD HH:MM:SS@.
--
-- @since 0.1
formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale localTimeFormat

-- | Parses the 'LocalTime' from @YYYY-MM-DD HH:MM:SS@. If the 'MonadFail'
-- instance throws an 'Control.Exception.Exception' consider
-- 'parseLocalTimeCallStack'.
--
-- @since 0.1
parseLocalTime :: MonadFail f => String -> f LocalTime
parseLocalTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

-- | Variant of 'parseLocalTime' that includes CallStack for thrown
-- exceptions.
--
-- @since 0.1
parseLocalTimeCallStack ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFail m
  ) =>
  String ->
  m LocalTime
parseLocalTimeCallStack = checkpointCallStack . parseLocalTime

-- | Parses the 'ZonedTime' from @YYYY-MM-DD HH:MM:SS Z@. If the 'MonadFail'
-- instance throws an 'Control.Exception.' consider
-- 'parseZonedTimeCallStack'.
--
-- @since 0.1
parseZonedTime :: MonadFail f => String -> f ZonedTime
parseZonedTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

-- | Variant of 'parseZonedTime' that includes CallStack for thrown
-- exceptions.
--
-- @since 0.1
parseZonedTimeCallStack ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFail m
  ) =>
  String ->
  m ZonedTime
parseZonedTimeCallStack = checkpointCallStack . parseZonedTime

localTimeFormat :: String
localTimeFormat = "%Y-%m-%d %H:%M:%S"

zonedTimeFormat :: String
zonedTimeFormat = "%Y-%m-%d %H:%M:%S %Z"

#if !MIN_VERSION_base(4,17,0)
properFractionDouble :: Integral b => Double -> (b, Double)
{-# NOINLINE [1] properFractionDouble #-}
properFractionDouble x =
  case decodeFloat x of
    (m, n) ->
      if n >= 0
        then (fromInteger m * 2 ^ n, 0.0)
        else case quotRem m (2 ^ negate n) of
          (w, r) ->
            (fromInteger w, encodeFloat r n)
#endif
