module DateTime
  ( range
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Data.Array as Array
import Data.DateTime (Date, DateTime, Hour, Minute)
import Data.DateTime as DateTime
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Time as Time
import LineDuration (LineDuration)
import LineDuration as LineDuration

range ::
  LineDuration -> OffsetDateTime -> OffsetDateTime -> Array OffsetDateTime
range duration b e =
  let
    headDate :: Date
    headDate = DateTime.date (OffsetDateTime.toLocalDateTime b)

    headZone :: TimeZoneOffset
    headZone = OffsetDateTime.timeZoneOffset b

    hour :: OffsetDateTime -> Hour
    hour = Time.hour <<< DateTime.time <<< OffsetDateTime.toLocalDateTime

    hours :: Array Hour
    hours = Enum.enumFromTo (hour b) (hour e)

    dt :: Date -> Hour -> Minute -> DateTime
    dt d h m = DateTime.DateTime d (DateTime.Time h m bottom bottom)

    odt :: TimeZoneOffset -> Date -> Hour -> Minute -> Maybe OffsetDateTime
    odt o d h m = OffsetDateTime.fromLocalDateTime o (dt d h m)

    odts :: TimeZoneOffset -> Date -> Hour -> Array OffsetDateTime
    odts o d h = Array.mapMaybe (odt o d h) (LineDuration.toMinutes duration)
  in
    Array.filter
      ((between
        (OffsetDateTimeFormatter.toString b)
        (OffsetDateTimeFormatter.toString e))
        <<< OffsetDateTimeFormatter.toString)
      (Array.concatMap (odts headZone headDate) hours)
