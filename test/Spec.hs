module Spec where

import Lib (computeBendersMoves, (|>))
import Test.HUnit
import CityMaps

testOfMap cityMap output =
  assertEqual
    (name cityMap)
    (computeBendersMoves $ sketch cityMap)
    output
  |> TestCase
  |> TestLabel (name cityMap)

test1 = testOfMap cityMap1
  ["SOUTH",
   "EAST",
   "NORTH",
   "EAST",
   "EAST"]

test2 = testOfMap cityMap2
  ["SOUTH",
   "EAST",
   "EAST",
   "EAST",
   "SOUTH",
   "EAST",
   "SOUTH",
   "SOUTH",
   "SOUTH"]

test3 = testOfMap cityMap3
  ["SOUTH",
   "SOUTH",
   "EAST",
   "EAST",
   "EAST",
   "NORTH",
   "NORTH",
   "NORTH",
   "NORTH",
   "NORTH"]

specs = TestList [test1, test2, test3]
