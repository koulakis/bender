module Spec where

import Lib (computeBendersMoves)
import Test.HUnit

test1 = TestCase $ assertEqual
  "Simple moves"
  (computeBendersMoves
    5
    6
    ["######",
     "#@E $#",
     "# N  #",
     "#X   #",
     "######"])
  ["SOUTH",
   "SOUTH",
   "EAST",
   "EAST"]

test2 = TestCase $ assertEqual
  "Obstacles"
  (computeBendersMoves
    10
    10
    ["########",
     "# @    #",
     "#     X#",
     "# XXX  #",
     "#   XX #",
     "#   XX #",
     "#     $#",
     "########"])
  ["SOUTH",
   "EAST",
   "EAST",
   "EAST",
   "SOUTH",
   "EAST",
   "SOUTH",
   "SOUTH",
   "SOUTH"]

test3 = TestCase $ assertEqual
  "Priorities"
  (computeBendersMoves
    8
    8
    ["########",
     "#     $#",
     "#      #",
     "#      #",
     "#  @   #",
     "#      #",
     "#      #",
     "########"])
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

specs = TestList [TestLabel "Simple moves" test1,
                  TestLabel "Obsticles" test2,
                  TestLabel "Priorities" test3]
