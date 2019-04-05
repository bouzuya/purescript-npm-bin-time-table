module Main
  ( main
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.DateTime as DateTimeFormatter
import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Data.Array as Array
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Minutes)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import DateTime as MyDateTime
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception as Exception
import Line (Line)
import Line as Line
import LineDuration (LineDuration)
import LineDuration as LineDuration
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Yargs.Applicative as Yargs
import Node.Yargs.Setup as YargsSetup

readStdin :: Effect String
readStdin = FS.readTextFile Encoding.UTF8 "/dev/stdin"

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
app "15min" = app' LineDuration.QuarterHour
app "30min" = app' LineDuration.HalfHour
app "1h" = app' LineDuration.OneHour
app line = Exception.throw (line <> " is not supported")

app' :: LineDuration -> Effect Unit
app' d = do
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
        dateTimes = MyDateTime.range d headOdt tailOdt
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
                      case
                        DateTime.adjust
                          (LineDuration.toDuration d :: Minutes)
                          b of
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

main :: Effect Unit
main = do
  let setup = YargsSetup.usage "Usage: $0 "
  Yargs.runY setup do
    app <$> Yargs.yarg "line" [] (Just "line") (Left "15min") true
