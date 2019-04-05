module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.DateTime as MyDateTime
import Test.Line as Line
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  MyDateTime.tests
  Line.tests
