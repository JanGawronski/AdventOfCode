module Main where
import Data.Bifunctor (bimap)

inputToList :: String -> [(Int, [Int])]
inputToList = map (bimap read (map read . words . tail) . span (/=':')) . lines

sat acc whole (number:list) = sat (acc * number) whole list || sat (acc + number) whole list
sat acc whole [] = whole == acc

main = do
  contents <- readFile "input.txt"
  print . sum . map fst. filter (uncurry $ sat 0) . inputToList $ contents
