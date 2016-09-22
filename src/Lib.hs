module Lib
    ( computeBendersMoves
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe
import Control.Monad.State
import Data.Set as Set

-- Pipes
(|>) x f = f x

-- Types
data Direction = S | E | N | W deriving (Show, Eq, Ord)
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
           obstaclesEaten :: Int} deriving (Show, Eq, Ord)
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
  |> concatMap (List.map readMapSymbol)
  |> zip [(x, y) | x <- [1..nLines],
                   y <- [1..nColumns]]
  |> Map.fromList
symbolPositionsInMap symbol cityMap =
  cityMap
  |> Map.filter (== symbol)
  |> Map.keys
readMapLocation location cityMap = fromMaybe undefined (Map.lookup location cityMap)

start = head . symbolPositionsInMap Start
teleporters = symbolPositionsInMap Teleporter

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
              |> List.filter (/= temporaryLocation)
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

stateUpdate (oldBenders, bender, cityMap) =
  let oldLocation = location bender
      oldBlock = readMapLocation oldLocation cityMap
      reorientedBender =
        newDirection
          (bender { heading =
                      case oldBlock of
                        NewHeading h -> h
                        _ -> heading bender})
          cityMap
      relocatedBender = newBenderPositionState reorientedBender cityMap
      newBenders = Set.insert bender oldBenders
      newState =
        case oldBlock of
          Obstacle ->
            (newBenders,
             relocatedBender { obstaclesEaten = obstaclesEaten bender + 1 },
             Map.insert oldLocation Empty cityMap)
          Death -> (newBenders, bender { alive = False }, cityMap)
          _ -> (newBenders, relocatedBender, cityMap)
  in (directionName $ heading bender, newState)

-- Run game
update :: State (Set Bender, Bender, CityMap) String
update = state stateUpdate

runGame = do
  (oldBenders, bender, cityMap) <- get
  if member bender oldBenders then return ["LOOP"]
  else if not $ alive bender then return []
  else do
    direction <- update
    restDirections <- runGame
    return (direction:restDirections)

computeBendersMoves :: Int -> Int -> [String] -> [String]
computeBendersMoves nLines nColumns stringMap =
  let cityMap = readMap nLines nColumns stringMap
      initialBender =
        Bender { inverted = False,
                 breakerMode = False,
                 location = start cityMap,
                 heading = S,
                 alive = True,
                 obstaclesEaten = 0}
  in evalState runGame (Set.empty, initialBender, cityMap)
