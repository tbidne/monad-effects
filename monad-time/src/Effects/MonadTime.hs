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
    parseZonedTime,
  )
where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime, ZonedTime)
import Data.Time.LocalTime qualified as Local
import GHC.Stack (HasCallStack)
import System.Clock (Clock (Monotonic), TimeSpec)
import System.Clock qualified as C

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

  -- | Retrieves the current 'TimeSpec', used for easy timing at the
  -- nanosecond level.
  --
  -- @since 0.5
  getTimeSpec :: m TimeSpec

-- | @since 0.1
instance MonadTime IO where
  getSystemTime = Local.zonedTimeToLocalTime <$> Local.getZonedTime
  getSystemZonedTime = Local.getZonedTime
  getTimeSpec = C.getTime Monotonic

-- | @since 0.1
instance MonadTime m => MonadTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getSystemZonedTime = lift getSystemZonedTime
  getTimeSpec = lift getTimeSpec

-- | Runs an action, returning the elapsed time.
--
-- @since 0.1
withTiming :: MonadTime m => m a -> m (TimeSpec, a)
withTiming m = do
  start <- getTimeSpec
  res <- m
  end <- getTimeSpec
  let diff = C.diffTimeSpec start end
  pure (diff, res)

-- | 'withTiming' but ignores the result value.
--
-- @since 0.1
withTiming_ :: MonadTime m => m a -> m TimeSpec
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

-- | Parses the 'LocalTime' from @YYYY-MM-DD HH:MM:SS@.
--
-- @since 0.1
parseLocalTime :: MonadFail f => String -> f LocalTime
parseLocalTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

-- | Parses the 'ZonedTime' from @YYYY-MM-DD HH:MM:SS Z@.
--
-- @since 0.1
parseZonedTime :: MonadFail f => String -> f ZonedTime
parseZonedTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat

localTimeFormat :: String
localTimeFormat = "%Y-%m-%d %H:%M:%S"

zonedTimeFormat :: String
zonedTimeFormat = "%Y-%m-%d %H:%M:%S %Z"