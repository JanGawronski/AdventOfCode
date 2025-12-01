module Main where

rotationToNumber rotation = read (tail rotation) * if head rotation == 'R' then 1 else -1

numbers = map rotationToNumber

integrate = scanl (+) 50

normalize = map (`mod` 100)

countZeros = length . filter (== 0)

main = do
    content <- readFile "input.txt"
    print . countZeros . normalize . integrate . numbers . lines $ content