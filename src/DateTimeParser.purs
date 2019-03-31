module DateTimeParser
  ( parseOffsetDateTime
  , parseTimeZoneOffset
  ) where

import Prelude

import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either as Either
import Data.Formatter.DateTime as Formatter
import Data.List as List
import Data.Maybe (Maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as RegexUnsafe
import Data.Time.Duration (Milliseconds)
import Data.Time.Duration as TimeDuration
import OffsetDateTime (OffsetDateTime)
import OffsetDateTime as OffsetDateTime
import TimeZoneOffsetFormat as TimeZoneOffsetFormat

parseDateTime :: String -> Maybe DateTime
parseDateTime s =
  Either.hush
    (Formatter.unformat
      (List.fromFoldable
        [ Formatter.YearFull
        , Formatter.Placeholder "-"
        , Formatter.MonthTwoDigits
        , Formatter.Placeholder "-"
        , Formatter.DayOfMonthTwoDigits
        , Formatter.Placeholder "T"
        , Formatter.Hours24
        , Formatter.Placeholder ":"
        , Formatter.MinutesTwoDigits
        , Formatter.Placeholder ":"
        , Formatter.SecondsTwoDigits
        ])
      s)

parseOffsetDateTime :: String -> Maybe OffsetDateTime
parseOffsetDateTime s = do
  let
    regex =
      RegexUnsafe.unsafeRegex
        "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2})(Z|[-+]\\d{2}:\\d{2})$"
        RegexFlags.noFlags
  matches <- Regex.match regex s
  dateTimeString <- join (NonEmptyArray.index matches 1)
  timeZoneOffsetString <- join (NonEmptyArray.index matches 2)
  dateTime <- parseDateTime dateTimeString
  timeZoneOffset <- parseTimeZoneOffset timeZoneOffsetString
  let
    ms :: Milliseconds
    ms = TimeZoneOffset.toDuration timeZoneOffset
  utcDateTime <- DateTime.adjust (TimeDuration.negateDuration ms) dateTime
  OffsetDateTime.offsetDateTime timeZoneOffset utcDateTime

parseTimeZoneOffset :: String -> Maybe TimeZoneOffset
parseTimeZoneOffset = TimeZoneOffsetFormat.fromString
