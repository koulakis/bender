module Lib where

import qualified Data.Map as Map
import Data.List as List
import Data.Maybe
import Control.Monad.State
import qualified Data.Set as Set
import Data.Function (on)

-- Custom tools
(|>) x f = f x
groupByUnordered transform collection =
  collection
  |> sortBy (compare `on` transform)
  |> groupBy ((==) `on` transform)

-- Types
data Direction = S | E | N | W deriving (Show, Eq, Ord)
data Block =
  Start
  | BenderCharacter
  | Empty
  | Wall
  | Obstacle
  | Death
  | NewHeading Direction
  | Beer
  | Invert
  | Teleporter deriving (Show, Eq, Ord)
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
  [('!', BenderCharacter),
   ('@', Start),
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
mapPrinter =
  symbolsAssosiation
  |> map (\(x, y) -> (y, x))
  |> Map.fromList
readMapSymbol symbol = fromMaybe undefined (Map.lookup symbol mapReader)
printMapSymbol symbol = fromMaybe undefined (Map.lookup symbol mapPrinter)
readMap stringArray =
  stringArray
  |> concatMap (map readMapSymbol)
  |> zip [(x, y) | y <- [1..(stringArray |> length)],
                   x <- [1..(stringArray |> head |> length)]]
  |> Map.fromList
printMap cityMap =
  cityMap
  |> Map.toList
  |> groupByUnordered (snd . fst)
  |> map (map (printMapSymbol . snd))
  |> intersperse "\n"
  |> (\l -> "\n":l)
  |> concat

symbolPositionsInMap symbol cityMap =
  cityMap
  |> Map.filter (== symbol)
  |> Map.keys
readMapLocation location cityMap = fromMaybe undefined (Map.lookup location cityMap)

start = head . symbolPositionsInMap Start
teleporters = symbolPositionsInMap Teleporter
initialBender cityMap =
  Bender { inverted = False,
           breakerMode = False,
           location = start cityMap,
           heading = S,
           alive = True,
           obstaclesEaten = 0}

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
    let currentDirection =
          case readMapLocation (location bender) cityMap of
            NewHeading d -> d
            _ -> heading bender
        priorities =
          if inverted bender then [W, N, E, S]
          else [S, E, N, W]
        finalDirection =
          if blocked bender cityMap currentDirection
          then find (not . blocked bender cityMap) priorities |> fromMaybe undefined
          else currentDirection
    in bender { heading = finalDirection }

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

stateUpdate currentState =
  let (bender, cityMap) = head currentState
      oldLocation = location bender
      oldBlock = readMapLocation oldLocation cityMap
      reorientedBender = newDirection bender cityMap
      relocatedBender = newBenderPositionState reorientedBender cityMap
      newBlock = readMapLocation (location relocatedBender) cityMap
      (newBender, newMap) =
        case (oldBlock, newBlock) of
          (_, Death) -> (bender { alive = False }, cityMap)
          (Obstacle, _) ->
            (relocatedBender { obstaclesEaten = obstaclesEaten bender + 1 },
             Map.insert oldLocation Empty cityMap)
          _ -> (relocatedBender, cityMap)
  in (directionName $ heading reorientedBender, (newBender, newMap):currentState)

undoMove currentState =
  if length currentState > 1
  then ("", tail currentState)
  else ("", currentState)

printState currentState =
  do let (bender, cityMap) = head currentState
     cityMap
      |> Map.insert (location bender) BenderCharacter
      |> printMap
      |> putStrLn

-- State manipulation
update = state stateUpdate
undo :: State [(Bender, CityMap)] String
undo = state undoMove
doNothing :: State [(Bender, CityMap)] String
doNothing = state (\currentState -> ("", currentState))

-- Run game
runGame = do
  currentState <- get
  let benders = map fst currentState
  let bender = head benders
  let sameConfigurationAppeared = length benders > 1 && elem bender (tail benders)
  if sameConfigurationAppeared then return ["LOOP"]
  else if not $ alive bender then return []
  else do
    direction <- update
    restDirections <- runGame
    return (direction:restDirections)

computeBendersMoves :: [String] -> [String]
computeBendersMoves stringMap =
  let cityMap = readMap stringMap
  in evalState runGame [(initialBender cityMap, cityMap)]
