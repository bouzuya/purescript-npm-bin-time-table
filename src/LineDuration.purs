module LineDuration
  ( LineDuration(..)
  , toDuration
  , toMinutes
  ) where

import Data.Array as Array
import Data.Enum as Enum
import Data.Time (Minute)
import Data.Time.Duration (class Duration)
import Data.Time.Duration as TimeDuration

data LineDuration
  = QuarterHour
  | HalfHour
  | OneHour

toDuration :: forall a. Duration a => LineDuration -> a
toDuration duration =
  TimeDuration.convertDuration
    case duration of
      QuarterHour -> TimeDuration.Minutes 15.0
      HalfHour -> TimeDuration.Minutes 30.0
      OneHour -> TimeDuration.Minutes 60.0

toMinutes :: LineDuration -> Array Minute
toMinutes duration =
  Array.mapMaybe
    Enum.toEnum
    (case duration of
      QuarterHour -> [0, 15, 30, 45]
      HalfHour -> [0, 30]
      OneHour -> [0])
