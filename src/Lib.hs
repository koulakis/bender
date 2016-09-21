module Lib
    ( computeBendersMoves
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

-- Pipes
(|>) x f = f x

-- Types
data Direction = S | E | N | W deriving (Show, Eq)
data Block =
  Start
  | Empty
  | Wall
  | Obstacle
  | Death
  | NewHeading Direction
  | Beer
  | Invert
  | Teleporter deriving (Show, Eq)
data Bender =
  Bender { inverted :: Bool,
           breakerMode :: Bool,
           location :: (Int, Int),
           heading :: Direction,
           alive :: Bool,
           obstaclesEaten :: Int} deriving (Show, Eq)
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
   (' ', Empty),
   ('#', Wall),
   ('X', Obstacle),
   ('$', Death),
   ('S', NewHeading S),
   ('E', NewHeading E),
   ('N', NewHeading N),
   ('W', NewHeading W),
   ('B', Beer),
   ('I', Invert),
   ('T', Teleporter) ]
mapReader = Map.fromList symbolsAssosiation
readMapSymbol symbol = fromMaybe undefined (Map.lookup symbol mapReader)
readMap nLines nColumns stringArray =
  stringArray
  |> concatMap (map readMapSymbol)
  |> zip [(x, y) | x <- [1..nLines],
                   y <- [1..nColumns]]
  |> Map.fromList
symbolPositionsInMap symbol cityMap =
  cityMap
  |> Map.filter (== symbol)
  |> Map.keys
readMapLocation location cityMap = fromMaybe undefined (Map.lookup location cityMap)

-- Calculating direction
blocked bender cityMap direction =
  let (x, y) = location bender
      checkIfBlocked nextLocation =
        let block = readMapLocation nextLocation cityMap
        in block == Wall || (block == Obstacle && not (breakerMode bender))
  in case direction of
      S -> checkIfBlocked (x, y + 1)
      E -> checkIfBlocked (x + 1, y)
      N -> checkIfBlocked (x, y - 1)
      W -> checkIfBlocked (x - 1, y)

newDirection bender cityMap =
    let currentDirection = heading bender
        priorities =
          if inverted bender then [W, N, E, S]
          else [S, E, N, W]
        newDirection =
          if blocked bender cityMap currentDirection
          then List.find (not . blocked bender cityMap) priorities |> fromMaybe undefined
          else currentDirection
    in bender { heading = newDirection }

-- Changing the state
start = head . symbolPositionsInMap Start
teleporters = symbolPositionsInMap Teleporter

newBenderPositionState bender cityMap =
  let (x, y) = location bender
      temporaryLocation =
        case heading bender of
          S -> (x, y + 1)
          E -> (x + 1, y)
          N -> (x, y - 1)
          W -> (x - 1, y)
      nextLocation =
        if readMapLocation temporaryLocation cityMap == Teleporter
        then teleporters cityMap
              |> filter (/= temporaryLocation)
              |> head
        else temporaryLocation
      nextBlock = readMapLocation nextLocation cityMap

      nextBreakerMode =
        if nextBlock == Beer
        then not $ breakerMode bender
        else breakerMode bender
      nextInverted =
        if nextBlock == Invert
        then not $ inverted bender
        else inverted bender
  in bender { location = nextLocation,
              breakerMode = nextBreakerMode,
              inverted = nextInverted }



computeBendersMoves :: Int -> Int -> [String] -> [String]
computeBendersMoves = undefined
