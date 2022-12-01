module Main where

import Data.Function
import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main =
  -- Read and print the contents of ../data/day1.txt
  do
    day1data <- readFile "data/day1.txt"
    print (day1 day1data)

day1 :: String -> Int
day1 day1data = do
  summedStrings day1data
    & sort
    & reverse
    & take 3
    & sum

summedStrings :: String -> [Int]
summedStrings contents =
  splitOn "\n\n" contents
    & map lines
    & map (sum . map read) ::
    [Int]