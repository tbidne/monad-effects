-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module Effects.MonadTime
  ( -- * Class
    MonadTime (..),

    -- * Timing
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
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)

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
  getMonotonicTime :: HasCallStack => m Natural

-- | @since 0.1
instance MonadTime IO where
  getSystemTime =
    checkpointCallStack
      (Local.zonedTimeToLocalTime <$> Local.getZonedTime)
  getSystemZonedTime = checkpointCallStack Local.getZonedTime
  getMonotonicTime = checkpointCallStack $ floor <$> C.getMonotonicTime

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getSystemZonedTime = lift getSystemZonedTime
  getMonotonicTime = lift getMonotonicTime

-- | Runs an action, returning the elapsed seconds.
--
-- @since 0.1
withTiming ::
  ( HasCallStack,
    MonadTime m
  ) =>
  m a ->
  m (Natural, a)
withTiming m = do
  start <- getMonotonicTime
  res <- m
  end <- getMonotonicTime
  pure (end - start, res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ ::
  ( HasCallStack,
    MonadTime m
  ) =>
  m a ->
  m Natural
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
