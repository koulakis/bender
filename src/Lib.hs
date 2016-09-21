module Lib
    ( computeBendersMoves
    ) where

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

computeBendersMoves :: Int -> Int -> [String] -> [String]
computeBendersMoves = undefined
