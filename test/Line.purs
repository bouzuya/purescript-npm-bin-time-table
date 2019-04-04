module Test.Line
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTimeFormatter
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple as Tuple
import Line as Line
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Line" do
  TestUnit.test "fromString" do
    let
      toString (Tuple.Tuple odt message) =
        String.joinWith
          " "
          [ OffsetDateTimeFormatter.toString odt
          , message
          ]

    let lineString = "2019-04-05T08:15:45+09:00 MESSAGE"
    Assert.equal
      (Maybe.Just lineString)
      (map toString (Line.fromString lineString))
