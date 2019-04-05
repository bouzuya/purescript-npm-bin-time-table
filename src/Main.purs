module Main
  ( main
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Data.Array as Array
import Data.DateTime (Date, DateTime, Hour, Minute)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time as Time
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception as Exception
import Line (Line)
import Line as Line
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Yargs.Applicative as Yargs
import Node.Yargs.Setup as YargsSetup

readStdin :: Effect String
readStdin = FS.readTextFile Encoding.UTF8 "/dev/stdin"

range :: OffsetDateTime -> OffsetDateTime -> Array OffsetDateTime
range b e =
  let
    headDate :: Date
    headDate = DateTime.date (OffsetDateTime.toUTCDateTime b)

    headZone :: TimeZoneOffset
    headZone = OffsetDateTime.timeZoneOffset b

    hour :: OffsetDateTime -> Hour
    hour = Time.hour <<< DateTime.time <<< OffsetDateTime.toUTCDateTime

    hours :: Array Hour
    hours = Enum.enumFromTo (hour b) (hour e)

    minutes :: Array Minute
    minutes = Array.mapMaybe Enum.toEnum [0, 15, 30, 45]

    dt :: Date -> Hour -> Minute -> DateTime
    dt d h m = DateTime.DateTime d (DateTime.Time h m bottom bottom)

    odt :: TimeZoneOffset -> Date -> Hour -> Minute -> Maybe OffsetDateTime
    odt o d h m = OffsetDateTime.fromUTCDateTime o (dt d h m)

    odts :: TimeZoneOffset -> Date -> Hour -> Array OffsetDateTime
    odts o d h = Array.mapMaybe (odt o d h) minutes
  in
    Array.concatMap (odts headZone headDate) hours

sortByOffsetDateTimeAsc :: Array Line -> Array Line
sortByOffsetDateTimeAsc lines =
  let
    compare =
      comparing
        (DateTimeFormatter.toString
          <<< OffsetDateTime.toUTCDateTime
          <<< Tuple.fst)
  in Array.sortBy compare lines

app :: String -> Effect Unit
app "15min" = do
  input <- readStdin
  let
    lines =
      Array.mapMaybe
        Line.fromString
        (String.split (String.Pattern "\n") (String.trim input))
    sorted = sortByOffsetDateTimeAsc lines
    hlMaybe = do
      head <- Array.head sorted
      last <- Array.last sorted
      pure { head, last }
  case hlMaybe of
    Nothing -> pure unit
    Just { head: (Tuple headOdt _), last: (Tuple tailOdt _) } -> do
      let
        -- TODO: Date
        date = DateTime.date <<< OffsetDateTime.toUTCDateTime
        time = DateTime.time <<< OffsetDateTime.toUTCDateTime
        headDate = date headOdt
        headTime = time headOdt
        headZone = OffsetDateTime.timeZoneOffset headOdt
        tailTime = time tailOdt
        dateTimes = range headOdt tailOdt
        outputLines =
          map
            (\odt ->
              Tuple
                odt
                (Array.filter
                  (\(Tuple i _) ->
                    let
                      b = OffsetDateTime.toUTCDateTime odt
                      i' = OffsetDateTime.toUTCDateTime i
                    in
                      case DateTime.adjust (Duration.Minutes 15.0) b of
                        Nothing -> false
                        Just e -> between b e i')
                  sorted))
            dateTimes

        toStringLine :: Tuple OffsetDateTime (Array Line) -> String
        toStringLine (Tuple odt ls) =
          let
            t =
              String.take
                (String.length "HH:MM")
                (String.drop
                  (String.length "YYYY-MM-DDT")
                  (OffsetDateTimeFormatter.toString odt))
            m =
              case Array.head ls of
                Nothing -> ""
                Just (Tuple _ h) ->
                  h <> (if Array.length ls > 1 then " ..." else "")
          in
            t <> " " <> m
      Console.log (String.joinWith "\n" (map toStringLine outputLines))

app line = Exception.throw (line <> " is not supported")

main :: Effect Unit
main = do
  let setup = YargsSetup.usage "Usage: $0 "
  Yargs.runY setup do
    app <$> Yargs.yarg "line" [] (Just "line") (Left "15min") true
