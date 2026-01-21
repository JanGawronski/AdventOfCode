module Main where
import Data.Map (fromList, lookup, filter, toList)

inputToMap = fromList . concat . zipWith (\y row -> zipWith (\x e -> ((x, y), read [e])) [1..] row) [1..] . lines

hike _ (_, 9) = 1
hike m ((x, y), h) = sum . map (\t -> hike m (t, h + 1)) $ Prelude.filter (\t -> Data.Map.lookup t m == Just (h + 1)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

main = do
  contents <- readFile "input.txt"
  let m = inputToMap contents
  print . sum $ map (hike m) (toList $ Data.Map.filter (==0) m)

