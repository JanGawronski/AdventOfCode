module Main where

inputToListOfLists = map (map (=='@')) . lines

get list index = if index >= 0 && index < length list
                then Just (list !! index)
                else Nothing

get2d list (x, y) = case get list y of
    Nothing  -> Nothing
    Just row -> get row x

countNearby list (x, y) = sum [if get2d list (x + i, y + j) == Just True then 1 else 0 | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0]

countAll list = sum [if countNearby list (x, y) < 4 && get2d list (x, y) == Just True then 1 else 0 | x <- [0..length (head list) - 1], y <- [0..length list]]


main = do
    contents <- readFile "input.txt"
    print . countAll . inputToListOfLists $ contents