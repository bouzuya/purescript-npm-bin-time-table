module Main
  ( main
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Data.Array as Array
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
    date = DateTime.date <<< OffsetDateTime.toUTCDateTime
    time = DateTime.time <<< OffsetDateTime.toUTCDateTime
    headDate = date b
    headTime = time b
    headZone = OffsetDateTime.timeZoneOffset b
    tailTime = time e
  in
    Array.concatMap
      (\h ->
        Array.mapMaybe
          (\min ->
            OffsetDateTime.fromUTCDateTime
              headZone
              (DateTime.DateTime
                headDate
                (DateTime.Time h min bottom bottom)))
          (Array.mapMaybe Enum.toEnum [0, 15, 30, 45]))
      (Enum.enumFromTo (Time.hour headTime) (Time.hour tailTime))

app :: String -> Effect Unit
app "15min" = do
  input <- readStdin
  let
    lines = String.split (String.Pattern "\n") (String.trim input)
    sorted =
      Array.sortBy
        (comparing (OffsetDateTimeFormatter.toString <<< Tuple.fst))
        (Array.mapMaybe Line.fromString lines)
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
