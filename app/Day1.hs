module Day1 where

import Data.Function
import Data.List (sort)
import Data.List.Split (splitOn)

-- 204610
run :: String -> Int
run day1data = do
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
