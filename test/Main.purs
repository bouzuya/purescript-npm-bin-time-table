module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Line as Line
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  Line.tests
