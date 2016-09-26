module Main where

import Lib
import CityMaps

import Control.Monad.State
import System.Console.ANSI
import qualified Data.Map as Map
import Data.List as List
import System.IO

printAvailableMaps availableMaps =
  putStrLn
    ("Please select one of the following maps:\n\n"
    ++
    (availableMaps
    |> zipWith
        (\x y -> show x ++ ". " ++ name y)
        [1..(length availableMaps)]
    |> intersperse "\n"
    |> concat))

getSelectedMap availableMaps =
  getLine
  |$ \selection ->
    selection
    |> (\x -> read x :: Int)
    |> \x -> availableMaps!!(x - 1)
    |> sketch
    |> readMap

selectMap availableMaps =
  printAvailableMaps availableMaps
  >> getSelectedMap availableMaps

chooseStateAction key =
  case key of
    'k' -> snd . undoMove
    'l' -> snd . stateUpdate
    _ -> id

mainLoop =
  lift clearScreen
  >> get
  >>= lift . printState
  >> lift getChar
  >>= modify . chooseStateAction
  >> get
  >> mainLoop

main :: IO()
main =
  hSetBuffering stdin NoBuffering
  >> hSetBuffering stdout NoBuffering
  >> selectMap cityMaps
  >>= \cityMap -> evalStateT mainLoop [(initialBender cityMap, cityMap)]
  >>= \x -> return ()
