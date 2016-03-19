module Test.Main where

import Prelude

import Test.Unit (test, runTest)
import Test.Unit.Assert as Assert

import Diag (answer)

main = runTest do
  test "arithmetic" do
    Assert.assert "answer should be 5.0" $ (answer) == 5.0
