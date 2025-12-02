module Main where
import Data.Bifunctor (bimap)
import Data.List (sort)

inputToList :: String -> [(Int, Int)]
inputToList = map (bimap read (read . tail) . span (/= '-')) . words . map (\x -> if x == ',' then ' ' else x) 

countDigits number = if number `div` 10 > 0 then 1 + countDigits (number `div` 10) else 1

repeatNumber number = number * (10 ^ countDigits number) + number

repeatedNumbers = map repeatNumber [1..]

aggregate :: Int -> [Int] -> [(Int,Int)] -> Int
aggregate acc [] _ = acc
aggregate acc _ [] = acc
aggregate acc (number:tail1) ((low, high):tail2) 
    | number < low = aggregate acc tail1 ((low, high):tail2)
    | number > high = aggregate acc (number:tail1) tail2
    | otherwise = aggregate (number + acc) tail1 ((low, high):tail2)




main = do
    contents <- readFile "input.txt"
    print . aggregate 0 repeatedNumbers . sort . inputToList $ contents