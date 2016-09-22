module Spec where

import Lib (computeBendersMoves)
import Test.HUnit

map1 =
  ["######",
   "#@E $#",
   "# N  #",
   "#X   #",
   "######"]
test1 = TestCase $ assertEqual
  "Simple moves"
  (computeBendersMoves map1)
  ["SOUTH",
   "EAST",
   "NORTH",
   "EAST",
   "EAST"]

map2 =
  ["########",
   "# @    #",
   "#     X#",
   "# XXX  #",
   "#   XX #",
   "#   XX #",
   "#     $#",
   "########"]
test2 = TestCase $ assertEqual
  "Obstacles"
  (computeBendersMoves map2)
  ["SOUTH",
   "EAST",
   "EAST",
   "EAST",
   "SOUTH",
   "EAST",
   "SOUTH",
   "SOUTH",
   "SOUTH"]

map3 =
  ["########",
   "#     $#",
   "#      #",
   "#      #",
   "#  @   #",
   "#      #",
   "#      #",
   "########"]
test3 = TestCase $ assertEqual
  "Priorities"
  (computeBendersMoves map3)
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
