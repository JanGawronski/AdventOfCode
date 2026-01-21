module Main where
import Data.Map (fromList, lookup, filter, toList)
import Data.List (nub)

inputToMap = fromList . concat . zipWith (\y row -> zipWith (\x e -> ((x, y), read [e])) [1..] row) [1..] . lines

hike _ ((x, y), 9) = [(x, y)]
hike m ((x, y), h) = concatMap (\t -> hike m (t, h + 1)) $ Prelude.filter (\t -> Data.Map.lookup t m == Just (h + 1)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

main = do
  contents <- readFile "input.txt"
  let m = inputToMap contents
  print . length $ concatMap (nub . hike m) (toList $ Data.Map.filter (==0) m)

