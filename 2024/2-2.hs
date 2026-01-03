module Main where

inputToListOfLists :: String -> [[Int]]
inputToListOfLists = map (map read . words) . lines

almostPairs f (x:y:z:list) = if f x y then almostPairs f (y:z:list)
                             else and $ zipWith f (x:z:list) (z:list) 
almostPairs _ _ = True

safe list = almostPairs (\x y -> x < y && y - x <= 3) list
  || almostPairs (\x y -> x > y && x - y <= 3) list
  || and (zipWith (\x y -> x < y && y - x <= 3) (tail list) (tail $ tail list))
  || and (zipWith (\x y -> x > y && x - y <= 3) (tail list) (tail $ tail list))  

main = do
  contents <- readFile "input.txt"
  print . length . filter safe . inputToListOfLists $ contents
