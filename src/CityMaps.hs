module CityMaps where

data MapInfo = MapInfo { name :: String, sketch :: [String]}

cityMap1 =
  MapInfo {
    name = "Corridor to death",
    sketch =
      ["###",
       "#@#",
       "# #",
       "# #",
       "# #",
       "# #",
       "#$#",
       "###"]}
cityMap2 =
  MapInfo {
    name = "Many direction changes",
    sketch =
      ["#################",
       "#               #",
       "#  @$       W   #",
       "#               #",
       "#  E        N   #",
       "#               #",
       "#               #",
       "#################"]}
cityMap3 =
  MapInfo {
    name = "Teleportation",
    sketch =
      ["#################",
       "#               #",
       "#  @            #",
       "#               #",
       "#               #",
       "#          T    #",
       "#               #",
       "#  T       $    #",
       "#               #",
       "#               #",
       "#################"]}
cityMap4 =
  MapInfo {
    name = "Breaker mode",
    sketch =
      ["#################",
       "#               #",
       "#  @            #",
       "#               #",
       "#               #",
       "#  B       $    #",
       "#               #",
       "#  E  XXXXXN    #",
       "#               #",
       "#               #",
       "#################"]}
cityMap5 =
  MapInfo {
    name = "Labyrinth",
    sketch =
      ["#################",
       "#               #",
       "# X@            #",
       "# X  XXXXXX     #",
       "# X  X$  IX     #",
       "# X  XXX  X     #",
       "# X       X     #",
       "# XXXXXXXXX     #",
       "#               #",
       "#               #",
       "#               #",
       "#################"]}
cityMap6 =
  MapInfo {
    name = "Crazy breaker",
    sketch =
      ["#################",
       "#               #",
       "#  @            #",
       "#               #",
       "#  B            #",
       "#  X            #",
       "#  X            #",
       "#  X            #",
       "#  B      X     #",
       "#  X     $      #",
       "#################"]}
cityMap7 =
  MapInfo {
    name = "Inversion of control",
    sketch =
      ["#################",
       "#               #",
       "#  @            #",
       "#               #",
       "#               #",
       "#  I            #",
       "#               #",
       "#  E   IX       #",
       "#               #",
       "#      $        #",
       "#################"]}
cityMap8 =
  MapInfo {
    name = "Loop forever!",
    sketch =
      ["#################",
       "#               #",
       "#  @      X     #",
       "#X        W     #",
       "#         NX    #",
       "# XX            #",
       "#               #",
       "#               #",
       "#               #",
       "#               #",
       "#################"]}
cityMap9 =
  MapInfo {
    name = "Eat the world...",
    sketch =
      ["###############################################",
       "#                                          X W#",
       "#  @                                        $ #",
       "#    X    X  XXXX    X      X       XX    X   #",
       "#  B X    X  X       X      X      X  X   X   #",
       "#  E XXXXXX  XXXX    X   B  X     X    X  X   #",
       "#    X    X  X       X      X      X  X       #",
       "#    X    X  XXXX    XXXXX  XXXXX   XX    X   #",
       "#                                             #",
       "#                                             #",
       "###############################################"]}
cityMap10 =
  MapInfo {
    name = "Dead end",
    sketch =
      ["#################",
       "#      X        #",
       "#  @   X        #",
       "#      X        #",
       "#XXXXXXX        #",
       "#               #",
       "#               #",
       "#       $       #",
       "#               #",
       "#               #",
       "#################"]}
cityMaps = [cityMap1, cityMap2, cityMap3, cityMap4, cityMap5,
            cityMap6, cityMap7, cityMap8, cityMap9, cityMap10]
