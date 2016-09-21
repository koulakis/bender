module Lib
    ( computeBendersMoves
    ) where

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
  Bender { inverted :: Bool
         , breakerMode :: Bool
         , location :: (Int, Int)
         , heading :: Direction
         , alive :: Bool
         , obstaclesEaten :: Int} deriving (Show)

-- Parsing, showing
directionName direction =
   case direction of
   S -> "SOUTH"
   E -> "EAST"
   N -> "NORTH"
   W -> "WEST"
readMapSymbol symbol =
  case symbol of
  '@' -> Start
  ' ' -> Empty
  '#' -> Wall
  'X' -> Obstacle
  '$' -> Death
  'S' -> NewHeading S
  'E' -> NewHeading E
  'N' -> NewHeading N
  'W' -> NewHeading W
  'B' -> Beer
  'I' -> Invert
  'T' -> Teleporter

computeBendersMoves :: Int -> Int -> [String] -> [String]
computeBendersMoves = undefined
