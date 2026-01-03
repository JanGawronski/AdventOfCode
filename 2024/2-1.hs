module Main where

inputToListOfLists :: String -> [[Int]]
inputToListOfLists = map (map read . words) . lines

safe list = (and (zipWith (>) list (tail list)) || and (zipWith (<) list (tail list))) && and (zipWith (\x y -> abs (x - y) <= 3) list (tail list))

main = do
  contents <- readFile "input.txt"
  print . length . filter safe . inputToListOfLists $ contents
