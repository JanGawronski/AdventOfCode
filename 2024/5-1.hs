module Main where
import Data.Bifunctor (Bifunctor(bimap))

splitOn acc ('\n':'\n':text) = (reverse acc, text)
splitOn acc (x:text) = splitOn (x:acc) text

inputToData :: [Char] -> ([(Int, Int)], [[Int]])
inputToData = bimap (map (bimap read (read . tail) . span (/='|')) . lines) (map (map read . words . map (\x -> if x == ',' then ' ' else x)) . lines) . splitOn ""

violatesRule (x:ordering) (first, second) | first == x = False
                                       | second == x = first `elem` ordering
                                       | otherwise = violatesRule ordering (first, second)
violatesRule _ _ = False

correct rules ordering = not $ any (violatesRule ordering) rules


main = do
  contents <- readFile "input.txt"
  let (rules, orderings) = inputToData contents
  print . sum . map (\x-> x !! (length x `div` 2)) . filter (correct rules) $ orderings
