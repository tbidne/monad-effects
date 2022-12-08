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
    withTiming,
    withTiming_,

    -- ** TimeSpec
    TimeSpec (..),

    -- *** Creation
    fromDouble,
    fromNanoSeconds,

    -- *** Elimination
    toDouble,
    toNanoSeconds,

    -- *** Operations
    diffTimeSpec,
    normalizeTimeSpec,

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
import Data.Bounds (LowerBounded (lowerBound), UpperBoundless)
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
import Numeric.Algebra
  ( AMonoid (zero),
    ASemigroup ((.+.)),
    MSemiSpace ((.*)),
    MSpace ((.%)),
    NonZero (MkNonZero),
    Normed (norm),
    Semimodule,
    SemivectorSpace,
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import Numeric.Literal.Rational (FromRational (afromRational))
import Optics.TH (makeFieldLabelsNoPrefix)

-- | Structure for holding time data. 'Eq' and 'Ord' are defined in terms of
-- an equivalence class e.g.
--
-- @
-- MkTimeSpec s n === MkTimeSpec 0 (s * 1_000_000_000 + n)
-- @
--
-- @since 0.1
data TimeSpec = MkTimeSpec
  { -- | Seconds.
    --
    -- @since 0.1
    sec :: !Natural,
    -- | Nanoseconds.
    --
    -- @since 0.1
    nsec :: !Natural
  }
  deriving stock
    ( -- | @since 0.1
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

-- | @since 0.1
instance Eq TimeSpec where
  l == r = toNanoSeconds l == toNanoSeconds r

-- | @since 0.1
instance Ord TimeSpec where
  l <= r = toNanoSeconds l <= toNanoSeconds r

-- | @since 0.1
instance LowerBounded TimeSpec where
  lowerBound = MkTimeSpec 0 0

-- | @since 0.1
instance UpperBoundless TimeSpec

-- | @since 0.1
instance FromInteger TimeSpec where
  afromInteger = fromNanoSeconds . fromInteger

-- | @since 0.1
instance FromRational TimeSpec where
  afromRational = fromDouble . fromRational

-- | @since 0.1
instance ASemigroup TimeSpec where
  MkTimeSpec ls ln .+. MkTimeSpec rs rn = MkTimeSpec (ls + rs) (ln + rn)

-- | @since 0.1
instance AMonoid TimeSpec where
  zero = MkTimeSpec 0 0

-- | @since 0.1
instance MSemiSpace TimeSpec Natural where
  MkTimeSpec s n .* k = MkTimeSpec (s * k) (n * k)

-- | @since 0.1
instance MSpace TimeSpec Natural where
  ts .% MkNonZero k = fromDouble (toDouble ts / fromIntegral k)

-- | @since 0.1
instance Semimodule TimeSpec Natural

-- | @since 0.1
instance SemivectorSpace TimeSpec Natural

-- | @since 0.1
instance Normed TimeSpec where
  norm = id

-- | Converts a 'Double' to a 'TimeSpec'.
--
-- @since 0.1
fromDouble :: Double -> TimeSpec
fromDouble d =
  MkTimeSpec
    { sec = seconds,
      nsec = nanoseconds
    }
  where
    (seconds, remainder) = properFractionDouble d
    nanoseconds = floor $ remainder * 1_000_000_000

-- | Converts 'Natural' nanoseconds to a 'TimeSpec'.
--
-- @since 0.1
fromNanoSeconds :: Natural -> TimeSpec
fromNanoSeconds nanoseconds = MkTimeSpec s ns
  where
    (s, ns) = quotRem nanoseconds 1_000_000_000

-- | Converts a 'TimeSpec' to a 'Double'.
--
-- @since 0.1
toDouble :: TimeSpec -> Double
toDouble (MkTimeSpec s n) =
  fromIntegral s + (fromIntegral n / 1_000_000_000)

-- | Converts a 'TimeSpec' into 'Natural' nanoseconds.
--
-- @since 0.1
toNanoSeconds :: TimeSpec -> Natural
toNanoSeconds (MkTimeSpec s n) = (s * 1_000_000_000) + n

-- | Returns the absolute difference of two 'TimeSpec's.
--
-- @since 0.1
diffTimeSpec :: TimeSpec -> TimeSpec -> TimeSpec
diffTimeSpec t1 t2
  | t1' >= t2' = fromDouble (t1' - t2')
  | otherwise = fromDouble (t2' - t1')
  where
    t1' = toDouble t1
    t2' = toDouble t2

-- | Normalizes nanoseconds < 1 second.
--
-- @since 0.1
normalizeTimeSpec :: TimeSpec -> TimeSpec
normalizeTimeSpec = fromNanoSeconds . toNanoSeconds

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
  pure (fromDouble (end - start), res)

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
