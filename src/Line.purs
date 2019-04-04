module Line
  ( Line
  , fromString
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Bouzuya.DateTime.OffsetDateTime (OffsetDateTime)
import Data.Maybe (Maybe)
import Data.String as String
import Data.Tuple (Tuple(..))

type Line = Tuple OffsetDateTime String

fromString :: String -> Maybe Line
fromString s = do
  index <- String.indexOf (String.Pattern " ") s
  let
    { after: messageWithSpace
    , before: offsetDateTimeString
    } = String.splitAt index s
    message = String.drop 1 messageWithSpace
  offsetDateTime <- OffsetDateTimeFormatter.fromString offsetDateTimeString
  pure (Tuple offsetDateTime message)
