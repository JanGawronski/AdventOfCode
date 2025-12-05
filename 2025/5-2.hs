module Main where
import Data.Bifunctor (bimap)
import Data.List (sort)

divide (x:y:input) = if x == y && y == '\n' then [] else x:divide (y:input)
divide list = list

rangesToList :: String -> [(Int, Int)]
rangesToList = map (bimap read (read . tail) . span (/= '-')) . words . map (\x -> if x == ',' then ' ' else x) 

mergeRanges ((x1, y1):(x2, y2):list) 
                                    | y1 >= y2 = mergeRanges ((x1, y1):list)
                                    | y1 >= x2 = mergeRanges ((x1, y2):list)
                                    | otherwise = (x1, y1):mergeRanges ((x2, y2):list)
mergeRanges list = list

sumRanges = sum . map (\(x, y) -> y - x + 1)


main = do
    contents <- readFile "input.txt"
    let ranges = sumRanges . mergeRanges . sort . rangesToList . divide $ contents
    print ranges