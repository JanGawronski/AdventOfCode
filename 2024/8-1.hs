module Main where
import Data.List (groupBy, sort, nub)
import Data.Function (on)

inputToList = concatMap (filter ((/= '.') . fst)) . zipWith (\y row -> zipWith (\x e -> (e, (x, y))) [1..] row) [1..] . lines

groupList = map (map snd) . groupBy ((==) `on` fst) . sort

antinodes list = [(x2 + (x2 - x1), y2 + (y2 - y1)) | (x1, y1) <- list, (x2, y2) <- list, (x1, y1) /= (x2, y2)]

main = do
  contents <- readFile "input.txt"
  let width = length . head . lines $ contents
  let height = length . lines $ contents
  print . length . nub . concatMap (filter (\(x, y) -> x >= 1 && x <= width && y >= 1 && y <= height) . antinodes) . groupList . inputToList $ contents

