module Test.Line
  ( tests
  ) where

import Prelude

import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Line" do
  TestUnit.test "fromString" do
    Assert.equal 0 0 -- TODO
    Assert.equal 1 1 -- TODO
