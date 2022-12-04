module Day3 where

import Data.Function
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Debug.Trace (trace)

-- [7742, 2276]
run :: String -> [Int]
run daydata = do
  let packs = lines daydata

  let part1 = map findDuplicate packs & countPriorities & sum
  let part2 = chunksOf 3 packs & map (head . intersect3) & countPriorities & sum
  [part1, part2]

intersect3 :: [String] -> String
intersect3 [a, b, c] = do
  intersect a b & intersect c

countPriorities :: [Char] -> [Int]
countPriorities duplicates =
  let lowerMap = zip ['a' .. 'z'] [1 .. 26] & Map.fromList
      upperMap = zip ['A' .. 'Z'] [27 .. 52] & Map.fromList
      combinedMap = Map.union lowerMap upperMap
   in duplicates
        & map (\ch -> Map.findWithDefault (-999999) ch combinedMap)

findDuplicate :: String -> Char
findDuplicate line =
  do
    let (firstHalf, secondHalf) = splitAt (length line `div` 2) line
    head $ intersect firstHalf secondHalf
