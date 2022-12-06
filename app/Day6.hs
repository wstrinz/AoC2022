module Day6 where

import Data.Function
import Data.List
import Debug.Trace (trace)
import Text.Regex.Posix ((=~))

run daydata = do
  let code = lines daydata & head

  let part1 = code & findFirstUniqueInWindow 4 0 Nothing
  let part2 = code & findFirstUniqueInWindow 14 0 Nothing
  [part1, part2]

findFirstUniqueInWindow :: Int -> Int -> Maybe Int -> String -> Int
findFirstUniqueInWindow windowSize currentIndex found code =
  case found of
    Just index -> index + windowSize
    Nothing ->
      do
        let currentWindowUniques = drop currentIndex code & take windowSize & nub

        let found' = if length currentWindowUniques == windowSize then Just currentIndex else Nothing

        findFirstUniqueInWindow windowSize (currentIndex + 1) found' code
