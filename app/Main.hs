module Main where

import Lib
import Control.Monad.State
import System.Console.ANSI
import Data.Map as Map
import System.IO

testMap =
  ["######",
   "#@E $#",
   "# N  #",
   "#X   #",
   "######"]

cityMap = readMap testMap
initialState = [(initialBender cityMap, cityMap)]

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
     mainLoop initialState
