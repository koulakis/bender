module Main where

import Lib
import CityMaps

import Control.Monad.State
import System.Console.ANSI
import qualified Data.Map as Map
import Data.List as List
import System.IO

selectMap :: [MapInfo] -> IO CityMap
selectMap availableMaps =
  do let names =
          availableMaps
          |> zipWith
              (\x y -> show x ++ ". " ++ name y)
              [1..(length availableMaps)]
          |> intersperse "\n"
          |> concat
     putStrLn $ "Please select one of the following maps:\n\n" ++ names
     selection <- getLine
     let selectedMap =
           selection
           |> (\x -> read x :: Int)
           |> \x -> availableMaps!!(x - 1)
           |> sketch
           |> readMap
     return selectedMap
     
chooseStateAction key =
  case key of
    'k' -> undo
    'l' -> Lib.update
    _ -> doNothing

singleAction currentState =
  do key <- getChar
     let newState = execState (chooseStateAction key) currentState
     clearScreen
     printState newState
     return newState

mainLoop currentState =
  do newState <- singleAction currentState
     mainLoop newState

main :: IO()
main =
  do hSetBuffering stdin NoBuffering
     hSetBuffering stdout NoBuffering
     cityMap <- selectMap cityMaps
     let initialState = [(initialBender cityMap, cityMap)]
     mainLoop initialState
