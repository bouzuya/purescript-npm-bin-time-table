module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.DateTimeFormatter as DateTimeFormatter
import Test.DateTimeParser as DateTimeParser
import Test.OffsetDateTime as OffsetDateTime
import Test.TimeZoneOffsetFormat as TimeZoneOffsetFormat
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  DateTimeFormatter.tests
  DateTimeParser.tests
  OffsetDateTime.tests
  TimeZoneOffsetFormat.tests
