module Test.Main where

import Prelude

import Main

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main :: Effect Unit
main = runTest do
  suite "Main" do
    test "todo" do
      Assert.equal 1 1
