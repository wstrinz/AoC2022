module Day5 where

import Control.Arrow ((>>>))
import Data.Function
import Data.List
import Data.List.Split (chunksOf, splitOn)
import Debug.Trace (trace)
import Text.Regex.Posix ((=~))

type Move = (Int, Int, Int)

type Stack = String

-- ["RNZLFZSJH","CNSFCGJSM"]
run :: String -> [String]
run daydata = do
  let [stacks, moves] = splitOn "\n\n" daydata
  let parsedStacks = parseStacks stacks
  let parsedMoves = map parseMove $ lines moves

  let part1 = foldl makeMove parsedStacks parsedMoves & map head
  let part2 = foldl makeMoveWithBulk parsedStacks parsedMoves & map head
  [part1, part2]

parseMove :: String -> Move
parseMove moves =
  let [amount, from, to] = moves =~ "[0-9]+" & map head :: [String]
   in (read amount, read from - 1, read to - 1)

parseStacks :: String -> [Stack]
parseStacks string =
  lines string
    & init
    & transpose
    & chunksOf 4
    & map ((!! 1) >>> dropWhile (== ' '))

makeMove :: [Stack] -> Move -> [Stack]
makeMove stacks (amount, from, to) =
  case amount of
    0 ->
      stacks
    _ ->
      let (fromStack, toStack) = (stacks !! from, stacks !! to)
          (fromStack', toStack') = (tail fromStack, head fromStack : toStack)
          stacks' = take from stacks ++ [fromStack'] ++ drop (from + 1) stacks
          stacks'' = take to stacks' ++ [toStack'] ++ drop (to + 1) stacks'
       in makeMove stacks'' (amount - 1, from, to)

makeMoveWithBulk :: [Stack] -> Move -> [Stack]
makeMoveWithBulk stacks (amount, from, to) =
  let (fromStack, toStack) = (stacks !! from, stacks !! to)
      fromStack' = drop amount fromStack
      toStack' = take amount fromStack ++ toStack
      stacks' = take from stacks ++ [fromStack'] ++ drop (from + 1) stacks
      stacks'' = take to stacks' ++ [toStack'] ++ drop (to + 1) stacks'
   in stacks''
