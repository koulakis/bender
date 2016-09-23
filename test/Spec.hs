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
  ["SOUTH","SOUTH","SOUTH","SOUTH","SOUTH"]
test2 = testOfMap cityMap2
  ["SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","NORTH","NORTH","WEST","WEST","WEST","WEST","WEST","WEST","WEST","WEST"]
test3 = testOfMap cityMap3
  ["SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","SOUTH"]
test4 = testOfMap cityMap4
  ["SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","NORTH","NORTH"]
test5 = testOfMap cityMap5
  ["SOUTH","SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","NORTH","NORTH","WEST","WEST","WEST"]
test6 = testOfMap cityMap6
  ["SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","SOUTH"]
test7 = testOfMap cityMap7
  ["SOUTH","SOUTH","SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","SOUTH","SOUTH"]
test8 = testOfMap cityMap8
  ["LOOP"]
test9 = testOfMap cityMap9
  ["SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","SOUTH","SOUTH","SOUTH","SOUTH","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","EAST","NORTH","NORTH","NORTH","NORTH","NORTH","NORTH","NORTH","NORTH","WEST","SOUTH"]
test10 = testOfMap cityMap10
  ["LOOP"]

specs = TestList [test1, test2, test3, test4, test5,
                  test6, test7, test8, test9, test10]
