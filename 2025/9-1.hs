module Main where
import Data.Bifunctor (bimap)

inputToList :: String -> [(Int, Int)]
inputToList = map (bimap read (read . tail) . span (/= ',')) . lines 

rectangleArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

biggestSquare points = maximum [rectangleArea a b | a <- points, b <- points]

main = do
    contents <- readFile "input.txt"
    print . biggestSquare . inputToList $ contents