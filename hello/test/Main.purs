module Test.Main where

import Prelude

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Math (pi)
import MyMath (answer, circleArea)

main = runTest do
  test "my math" do
    Assert.assert "answer should be 5.0" $ (answer) == 5.0
    Assert.assert "circleArea 1 should equal to pi" $ (circleArea 1.0) == pi
