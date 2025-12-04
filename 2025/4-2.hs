module Main where
import Data.Map (fromList, union, empty, findWithDefault, keys, insert, filter)


inputToListOfLists = map (map (=='@')) . lines

rowToMap row y = fromList $ zipWith (\element x -> ((x, y), element)) row [1..]

listOfListsToMap list = foldr union empty $ zipWith rowToMap list [1..]

full = findWithDefault False

fullNeigbours (x, y) list = [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0, full (x + i, y + j) list]

countNearby position list = length $ fullNeigbours position list

canBeRemoved position list = countNearby position list < 4 && full position list

remove position = insert position False

propagate (x, y) list = if canBeRemoved (x, y) list 
                        then foldr propagate (remove (x, y) list) (fullNeigbours (x, y) list) 
                        else list

main = do
    contents <- readFile "input.txt"
    let input = listOfListsToMap . inputToListOfLists $ contents
    let removed = foldr propagate input (keys input)
    let count = length (Data.Map.filter id input) - length (Data.Map.filter id removed)
    print count
