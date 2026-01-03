module Main where
import Data.Bifunctor (bimap)
import Data.List (transpose, sort)

inputTolist :: String -> [(Int, Int)]
inputTolist = map (bimap read read . span (/= ' ')) . lines

sortTupleList = (\(x:y:_) -> (x, y)) . map sort . transpose . map (\(x, y) -> [x, y])

distances = zipWith (\x y -> abs (x - y)) 

main = do
  contents <- readFile "input.txt"
  print . sum . uncurry distances . sortTupleList . inputTolist $ contents

