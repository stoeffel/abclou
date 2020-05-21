module Test.Main where

{-- import Game --}

import Data.Unit (Unit)

import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main :: Effect Unit
main = runTest do
  suite "Main" do
    test "todo" do
      Assert.equal 1 1
