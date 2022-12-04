module Main where

import Data.Function ()
import Day1 (run)
import Day2 (run)
import Day3 (run)
import System.Environment (getArgs)

main :: IO ()
main =
  do
    args <- getArgs
    day1data <- readFile "data/day1.txt"
    day2data <- readFile "data/day2.txt"
    day3data <- readFile "data/day3.txt"

    case args of
      ["1"] ->
        print $ Day1.run day1data
      ["2"] ->
        print $ Day2.run day2data
      ["3"] ->
        print $ Day3.run day3data
      _ ->
        do
          print "running all days"
          print (Day1.run day1data)
          print (Day2.run day2data)
          print (Day3.run day3data)
