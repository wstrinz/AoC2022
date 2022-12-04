module Day4 where

import Data.Function
import Data.IntSet (isSubsetOf)
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Regex.Posix ((=~))

run :: String -> [Int]
run daydata = do
  let seats = lines daydata

  let part1 = map parseLine seats & filter oneFullyContainsOther & length
  let part2 = map parseLine seats & filter intersectAtAll & length
  [part1, part2]

intersectAtAll :: (Int, Int, Int, Int) -> Bool
intersectAtAll (aStart, aEnd, bStart, bEnd) = do
  let a = [aStart .. aEnd]
      b = [bStart .. bEnd]
  intersect a b & length & (> 0)

oneFullyContainsOther :: (Int, Int, Int, Int) -> Bool
oneFullyContainsOther (aStart, aEnd, bStart, bEnd) =
  let a = [aStart .. aEnd]
      b = [bStart .. bEnd]
      inters = intersect a b
   in inters == a || inters == b

parseLine :: String -> (Int, Int, Int, Int)
parseLine str =
  -- Use a regex to extract the four numbers from the string
  let [a, b, c, d] = str =~ "[0-9]+" & map head :: [String]
   in -- Convert the extracted strings to Ints using read
      (read a, read b, read c, read d)