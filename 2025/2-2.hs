module Main where
import Data.Bifunctor (bimap)
import Data.List (sort)

inputToList :: String -> [(Int, Int)]
inputToList = map (bimap read (read . tail) . span (/= '-')) . words . map (\x -> if x == ',' then ' ' else x) 

countDigits number = if number `div` 10 > 0 then 1 + countDigits (number `div` 10) else 1

repeatNumber 1 number = number
repeatNumber times number = repeatNumber (times - 1) number * (10 ^ countDigits number) + number

repeatedNumbers times = map (repeatNumber times) [1..]

aggregate :: Int -> [Int] -> [(Int,Int)] -> Int
aggregate acc [] _ = acc
aggregate acc _ [] = acc
aggregate acc (number:tail1) ((low, high):tail2) 
    | number < low = aggregate acc tail1 ((low, high):tail2)
    | number > high = aggregate acc (number:tail1) tail2
    | otherwise = aggregate (number + acc) tail1 ((low, high):tail2)


aggregateAll times acc ranges = if numbers == 0 then acc else aggregateAll (tail times) (acc + numbers) ranges
                                where
                                    numbers = aggregate 0 (repeatedNumbers $ head times) ranges

combine lists = mn:combine (map (\x -> if head x == mn then tail x else x) lists)
                where 
                    mn = minimum $ map head lists


combinedRepeats = combine $ map repeatedNumbers [2..7]


main = do
    contents <- readFile "input.txt"
    print . aggregate 0 combinedRepeats . sort . inputToList $ contents