module Main where

import Data.Function
import Data.List (sort)
import Data.List.Split (splitOn)
import Debug.Trace

main :: IO ()
main =
  -- Read and print the contents of ../data/day1.txt
  do
    day1data <- readFile "data/day1.txt"
    day2data <- readFile "data/day2.txt"
    print (day1 day1data)
    print (day2 day2data)

-- 204610
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
    & map ((sum . map read) . lines) ::
    [Int]

-- [13682,12881]
day2 :: String -> [Int]
day2 day2data = do
  let charLists = lines day2data & map (take 3)
  let part1 = map scoreLine charLists & sum
  let part2 = map findResponse charLists & scoreWithResponses charLists & sum
  [part1, part2]

scoreWithResponses :: [[Char]] -> [Char] -> [Int]
scoreWithResponses =
  zipWith
    ( \charList response -> scoreLine (take 2 charList ++ [response])
    )

findResponse :: [Char] -> Char
findResponse line =
  let [theirs, _, outcome] = line
   in case outcome of
        'X' -> case theirs of
          'A' -> 'Z'
          'B' -> 'X'
          'C' -> 'Y'
        'Y' -> case theirs of
          'A' -> 'X'
          'B' -> 'Y'
          'C' -> 'Z'
        'Z' -> case theirs of
          'A' -> 'Y'
          'B' -> 'Z'
          'C' -> 'X'
        _ -> '?'

scoreLine :: [Char] -> Int
scoreLine line = do
  let [theirs, _, mine] = line
  let roundScore = scoreRound [mine] [theirs]
  let shapeScore = scoreShape [mine]
  roundScore + shapeScore

scoreShape :: [Char] -> Int
scoreShape shape =
  case shape of
    "X" -> 1
    "Y" -> 2
    "Z" -> 3
    _ -> trace ("DEBUG: shape" ++ show shape) (-100000)

scoreRound :: String -> String -> Int
scoreRound mine theirs =
  -- If mine is the same as theirs, return 3
  -- If mine is a win, return 6
  -- If mine is a loss, return 0
  case mine of
    "X" -> case theirs of
      "C" -> 6
      "A" -> 3
      "B" -> 0
    "Z" -> case theirs of
      "C" -> 3
      "A" -> 0
      "B" -> 6
    "Y" -> case theirs of
      "C" -> 0
      "A" -> 6
      "B" -> 3
    _ -> trace ("DEBUG: mine" ++ show mine) (-100000)
