module Line
  ( Line
  , fromString
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import DateTimeParser as DateTimeParser
import OffsetDateTime (OffsetDateTime)

type Line = Tuple OffsetDateTime String

fromString :: String -> Maybe Line
fromString s = do
  index <- String.indexOf (String.Pattern " ") s
  let
    { after: messageWithSpace
    , before: dateTimeString
    } = String.splitAt index s
    message = String.drop 1 messageWithSpace
  offsetDateTime <- DateTimeParser.parseOffsetDateTime dateTimeString
  pure (Tuple offsetDateTime message)
