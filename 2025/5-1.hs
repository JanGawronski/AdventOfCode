module Main where
import Data.Bifunctor (bimap)

divide (x:y:input) acc = if x == y && y == '\n' then (reverse acc, input) else divide (y:input) (x:acc)
divide list acc = (reverse acc ++ list, [])

rangesToList :: String -> [(Int, Int)]
rangesToList = map (bimap read (read . tail) . span (/= '-')) . words . map (\x -> if x == ',' then ' ' else x) 

idsToList :: String -> [Int]
idsToList = map read . lines 

inRange element = any (\(x, y) -> element >= x && element <= y)


main = do
    contents <- readFile "input.txt"
    let (ranges, numbers) = bimap rangesToList idsToList $ divide contents []
    let elementsInRange = length $ filter (`inRange` ranges) numbers
    print elementsInRange