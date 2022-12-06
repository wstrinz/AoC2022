module Main where

import Control.Applicative (pure)
import Data.Function ((&))
import Data.Functor
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Day1 (run)
import Day2 (run)
import Day3 (run)
import Day4 (run)
import Day5 (run)
import Day6 (run)
import System.Environment (getArgs)

main :: IO ()
main =
  do
    args <- getArgs
    day1data <- readFile "data/day1.txt"
    day2data <- readFile "data/day2.txt"
    day3data <- readFile "data/day3.txt"

    let day = args & listToMaybe & maybe 'a' head & pure
    dayData <- readFile $ "data/day" ++ day ++ ".txt"

    case day of
      "1" ->
        print $ Day1.run dayData
      "2" ->
        print $ Day2.run dayData
      "3" ->
        print $ Day3.run dayData
      "4" ->
        print $ Day4.run dayData
      "5" ->
        print $ Day5.run dayData
      "6" ->
        print $ Day6.run dayData
      _ ->
        do
          print "running first 3 days"
          print (Day1.run day1data)
          print (Day2.run day2data)
          print (Day3.run day3data)
