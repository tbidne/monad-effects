{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: [Equality Constraints]
--
-- Evidently, GHC 9.2 requires GADTs or TypeFamilies to use equality
-- constraints e.g. @k ~ A_Lens@.
#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 904
{-# LANGUAGE TypeFamilies #-}
#endif

-- | Provides the 'MonadTime' class.
--
-- @since 0.1
module Effects.Time
  ( -- * Effect
    MonadTime (..),
    getSystemTime,

    -- * Timing
    withTiming,
    withTiming_,

    -- ** TimeSpec
    TimeSpec (..),

    -- *** Creation
    fromSeconds,
    fromNanoSeconds,

    -- *** Elimination
    toSeconds,
    toNanoSeconds,

    -- *** Operations
    diffTimeSpec,
    normalizeTimeSpec,

    -- * Formatting
    formatLocalTime,
    formatZonedTime,

    -- * Parsing
    parseLocalTime,
    parseZonedTime,

    -- * Misc
    getSystemTimeString,
    getSystemZonedTimeString,

    -- * Reexports

    -- ** Time
    LocalTime (..),
    TimeZone,
    UTCTime,
    ZonedTime (..),

    -- ** TZ
    TZ,

    -- ** Algebra
    ASemigroup (..),
    AMonoid (..),
    MSemiSpace (..),
    MSpace (..),
    Semimodule,
    SemivectorSpace,
    Normed,
    LowerBounded (..),
    UpperBoundless,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bounds (LowerBounded (lowerBound), UpperBoundless)
import Data.Time.Clock (UTCTime)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime (LocalTime, localDay, localTimeOfDay),
    TimeZone,
    ZonedTime (ZonedTime, zonedTimeToLocalTime, zonedTimeZone),
  )
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as TZ
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
    Normed (norm, sgn),
    Semimodule,
    SemivectorSpace,
  )
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

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
instance
  (k ~ A_Lens, a ~ Natural, b ~ Natural) =>
  LabelOptic "sec" k TimeSpec TimeSpec a b
  where
  labelOptic = lensVL $ \f (MkTimeSpec a1 a2) ->
    fmap (\b -> MkTimeSpec b a2) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Natural, b ~ Natural) =>
  LabelOptic "nsec" k TimeSpec TimeSpec a b
  where
  labelOptic = lensVL $ \f (MkTimeSpec a1 a2) ->
    fmap (\b -> MkTimeSpec a1 b) (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance Eq TimeSpec where
  l == r = toNanoSeconds l == toNanoSeconds r
  {-# INLINEABLE (==) #-}

-- | @since 0.1
instance Ord TimeSpec where
  l <= r = toNanoSeconds l <= toNanoSeconds r
  {-# INLINEABLE (<=) #-}

-- | @since 0.1
instance LowerBounded TimeSpec where
  lowerBound = MkTimeSpec 0 0
  {-# INLINEABLE lowerBound #-}

-- | @since 0.1
instance UpperBoundless TimeSpec

-- | @since 0.1
instance ASemigroup TimeSpec where
  MkTimeSpec ls ln .+. MkTimeSpec rs rn = MkTimeSpec (ls + rs) (ln + rn)
  {-# INLINEABLE (.+.) #-}

-- | @since 0.1
instance AMonoid TimeSpec where
  zero = MkTimeSpec 0 0
  {-# INLINEABLE zero #-}

-- | @since 0.1
instance MSemiSpace TimeSpec Natural where
  MkTimeSpec s n .* k = MkTimeSpec (s * k) (n * k)
  {-# INLINEABLE (.*) #-}

-- | @since 0.1
instance MSpace TimeSpec Natural where
  ts .% k = fromSeconds (toSeconds ts / fromIntegral k)
  {-# INLINEABLE (.%) #-}

-- | @since 0.1
instance Semimodule TimeSpec Natural

-- | @since 0.1
instance SemivectorSpace TimeSpec Natural

-- | @since 0.1
instance Normed TimeSpec where
  norm = id
  {-# INLINEABLE norm #-}

  sgn x
    | x == zero = zero
    | otherwise = MkTimeSpec 1 0
  {-# INLINEABLE sgn #-}

-- | Converts 'Double' seconds to a 'TimeSpec'.
--
-- @since 0.1
fromSeconds :: Double -> TimeSpec
fromSeconds d =
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

-- | Converts a 'TimeSpec' to 'Double' seconds.
--
-- @since 0.1
toSeconds :: TimeSpec -> Double
toSeconds (MkTimeSpec s n) =
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
  | t1' >= t2' = fromNanoSeconds (t1' - t2')
  | otherwise = fromNanoSeconds (t2' - t1')
  where
    t1' = toNanoSeconds t1
    t2' = toNanoSeconds t2

-- | Normalizes nanoseconds < 1 second.
--
-- @since 0.1
normalizeTimeSpec :: TimeSpec -> TimeSpec
normalizeTimeSpec = fromNanoSeconds . toNanoSeconds

-- | Time effect.
--
-- @since 0.1
class (Monad m) => MonadTime m where
  -- | Returns the zoned system time.
  --
  -- @since 0.1
  getSystemZonedTime :: (HasCallStack) => m ZonedTime

  -- | Lifted 'Local.getTimeZone'.
  --
  -- @since 0.1
  getTimeZone :: (HasCallStack) => UTCTime -> m TimeZone

  -- | Lifted 'Local.utcToLocalZonedTime'.
  --
  -- @since 0.1
  utcToLocalZonedTime :: (HasCallStack) => UTCTime -> m ZonedTime

  -- | Lifted 'TZ.loadLocalTZ'.
  --
  -- @since 0.1
  loadLocalTZ :: (HasCallStack) => m TZ

  -- | Return monotonic time in seconds, since some unspecified starting
  -- point.
  --
  -- @since 0.1
  getMonotonicTime :: (HasCallStack) => m Double

-- | @since 0.1
instance MonadTime IO where
  getSystemZonedTime = Local.getZonedTime
  {-# INLINEABLE getSystemZonedTime #-}
  getTimeZone = Local.getTimeZone
  {-# INLINEABLE getTimeZone #-}
  utcToLocalZonedTime = Local.utcToLocalZonedTime
  {-# INLINEABLE utcToLocalZonedTime #-}
  loadLocalTZ = TZ.loadLocalTZ
  {-# INLINEABLE loadLocalTZ #-}
  getMonotonicTime = C.getMonotonicTime
  {-# INLINEABLE getMonotonicTime #-}

-- | @since 0.1
instance (MonadTime m) => MonadTime (ReaderT e m) where
  getSystemZonedTime = lift getSystemZonedTime
  {-# INLINEABLE getSystemZonedTime #-}
  getTimeZone = lift . getTimeZone
  {-# INLINEABLE getTimeZone #-}
  utcToLocalZonedTime = lift . utcToLocalZonedTime
  {-# INLINEABLE utcToLocalZonedTime #-}
  loadLocalTZ = lift loadLocalTZ
  {-# INLINEABLE loadLocalTZ #-}
  getMonotonicTime = lift getMonotonicTime
  {-# INLINEABLE getMonotonicTime #-}

-- | Returns the local system time.
--
-- @since 0.1
getSystemTime :: (HasCallStack, MonadTime m) => m LocalTime
getSystemTime = Local.zonedTimeToLocalTime <$> getSystemZonedTime
{-# INLINEABLE getSystemTime #-}

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
  pure (fromSeconds (end - start), res)
{-# INLINEABLE withTiming #-}

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
{-# INLINEABLE withTiming_ #-}

-- TODO: handle more time zones?

-- | Formats the 'ZonedTime' to @YYYY-MM-DD HH:MM:SS Z@.
--
-- @since 0.1
formatZonedTime :: ZonedTime -> String
formatZonedTime = Format.formatTime Format.defaultTimeLocale zonedTimeFormat

-- | Retrieves the formatted 'LocalTime'.
--
-- @since 0.1
getSystemTimeString :: (HasCallStack, MonadTime m) => m String
getSystemTimeString = fmap formatLocalTime getSystemTime
{-# INLINEABLE getSystemTimeString #-}

-- | Formats the 'LocalTime' to @YYYY-MM-DD HH:MM:SS@.
--
-- @since 0.1
formatLocalTime :: LocalTime -> String
formatLocalTime = Format.formatTime Format.defaultTimeLocale localTimeFormat

-- | Retrieves the formatted 'ZonedTime'.
--
-- @since 0.1
getSystemZonedTimeString :: (HasCallStack, MonadTime m) => m String
getSystemZonedTimeString = fmap formatZonedTime getSystemZonedTime
{-# INLINEABLE getSystemZonedTimeString #-}

-- TODO: It would be nice if parse(Local|Zoned)Time included a callstack
-- when f is IO. Alas, IO's MonadFail instance uses failIO, which does NOT
-- add backtrace information.
--
-- Keeping this note in case this changes.
--
-- Update: There is now an issue for this:
--
--     https://github.com/haskell/core-libraries-committee/issues/301
--
-- If this is merged, add HasCallStack to most (all?) functions in this
-- repo w/ MonadFail.

-- | Parses the 'LocalTime' from @YYYY-MM-DD HH:MM:SS@.
--
-- @since 0.1
parseLocalTime :: (MonadFail f) => String -> f LocalTime
parseLocalTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    localTimeFormat
{-# INLINEABLE parseLocalTime #-}

-- | Parses the 'ZonedTime' from @YYYY-MM-DD HH:MM:SS Z@.
--
-- ==== __Known Timezones__
--
-- * UTC
-- * UT
-- * GMT
-- * EST
-- * EDT
-- * CST
-- * CDT
-- * MST
-- * MDT
-- * PST
-- * PDT
-- * +HHMM (e.g. +1300)
--
-- @since 0.1
parseZonedTime :: (MonadFail f) => String -> f ZonedTime
parseZonedTime =
  Format.parseTimeM
    True
    Format.defaultTimeLocale
    zonedTimeFormat
{-# INLINEABLE parseZonedTime #-}

localTimeFormat :: String
localTimeFormat = "%0Y-%m-%d %H:%M:%S"

zonedTimeFormat :: String
zonedTimeFormat = "%0Y-%m-%d %H:%M:%S %Z"

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
