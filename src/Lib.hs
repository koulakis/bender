module Lib
    ( computeBendersMoves
    ) where

import qualified Data.Map as Map
import Data.Maybe
-- Pipes
(|>) x f = f x

-- Types
data Direction = S | E | N | W deriving (Show)
data Block =
  Start
  | Empty
  | Wall
  | Obstacle
  | Death
  | NewHeading Direction
  | Beer
  | Invert
  | Teleporter deriving (Show)
data Bender =
  Bender { inverted :: Bool,
           breakerMode :: Bool,
           location :: (Int, Int),
           heading :: Direction,
           alive :: Bool,
           obstaclesEaten :: Int} deriving (Show)
type CityMap = Map.Map (Int, Int) Block

-- Parsing, showing
directionName direction =
   case direction of
   S -> "SOUTH"
   E -> "EAST"
   N -> "NORTH"
   W -> "WEST"
symbolsAssosiation =
  [('@', Start),
   (' ',  Empty),
   ('#',  Wall),
   ('X',  Obstacle),
   ('$',  Death),
   ('S',  NewHeading S),
   ('E',  NewHeading E),
   ('N',  NewHeading N),
   ('W',  NewHeading W),
   ('B',  Beer),
   ('I',  Invert),
   ('T',  Teleporter) ]
mapReader = Map.fromList symbolsAssosiation
readMapSymbol symbol = fromMaybe undefined (Map.lookup symbol mapReader)
readMap nLines nColumns stringArray =
  stringArray
  |> concatMap (map readMapSymbol)
  |> zip [(x, y) | x <- [1..nLines],
                   y <- [1..nColumns]]
  |> Map.fromList
symbolPositionsInMap symbol cityMap = cityMap |> Map.filter (== symbol)


computeBendersMoves :: Int -> Int -> [String] -> [String]
computeBendersMoves = undefined
