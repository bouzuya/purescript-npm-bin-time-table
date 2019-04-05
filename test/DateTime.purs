module Test.DateTime
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.Formatter.OffsetDateTime as OffsetDateTime
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import DateTime as MyDateTime
import LineDuration as LineDuration
import Partial.Unsafe as Unsafe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "DateTime" do
  TestUnit.test "range" do
    let
      b =
        Unsafe.unsafePartial
          (Maybe.fromJust
            (OffsetDateTime.fromString "2019-04-05T08:00:00+09:00"))
      e =
        Unsafe.unsafePartial
          (Maybe.fromJust
            (OffsetDateTime.fromString "2019-04-05T09:30:00+09:00"))
    Assert.equal
      (Array.mapMaybe
        Just
        [ "2019-04-05T08:00:00+09:00"
        , "2019-04-05T08:15:00+09:00"
        , "2019-04-05T08:30:00+09:00"
        , "2019-04-05T08:45:00+09:00"
        , "2019-04-05T09:00:00+09:00"
        , "2019-04-05T09:15:00+09:00"
        , "2019-04-05T09:30:00+09:00"
        ])
      (map
        OffsetDateTime.toString
        (MyDateTime.range LineDuration.QuarterHour b e))
