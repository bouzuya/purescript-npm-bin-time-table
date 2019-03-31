module Test.DateTimeParser
  ( tests
  ) where

import Prelude

import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "DateTimeParser" do
  TestUnit.test "parseDateTime" do
    Assert.equal 1 1 -- TODO

  TestUnit.test "parseOffsetDateTime" do
    Assert.equal 1 1 -- TODO

  TestUnit.test "parseTimeZone" do
    Assert.equal 1 1 -- TODO
