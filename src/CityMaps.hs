module CityMaps where

data MapInfo = MapInfo { name :: String, sketch :: [String]}

cityMap1 =
  MapInfo {
    name = "Simple moves",
    sketch =
      ["######",
       "#@E $#",
       "# N  #",
       "#X   #",
       "######"]}
cityMap2 =
  MapInfo {
    name = "Obstacles",
    sketch =
      ["########",
       "# @    #",
       "#     X#",
       "# XXX  #",
       "#   XX #",
       "#   XX #",
       "#     $#",
       "########"]}
cityMap3 =
  MapInfo {
    name = "Priorities",
    sketch =
      ["########",
       "#     $#",
       "#      #",
       "#      #",
       "#  @   #",
       "#      #",
       "#      #",
       "########"]}

cityMaps = [cityMap1, cityMap2, cityMap3]
