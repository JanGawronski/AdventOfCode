module Main where
import Data.Bifunctor (bimap)

inputToList :: String -> [(Int, [Int])]
inputToList = map (bimap read (map read . words . tail) . span (/=':')) . lines

numberSize number = if number < 10 then 1 else 1 + numberSize (number `div` 10)

concatNumbers n1 n2 = n1 * (10 ^ numberSize n2) + n2

sat acc whole (number:list) = (acc <= whole) && (sat (concatNumbers acc number) whole list || sat (acc * number) whole list || sat (acc + number) whole list)
sat acc whole [] = whole == acc

main = do
  contents <- readFile "input.txt"
  print . sum . map fst. filter (uncurry $ sat 0) . inputToList $ contents
