module Main where

rotationToNumber rotation = read (tail rotation) * if head rotation == 'R' then 1 else -1

numbers = map rotationToNumber

integrateCount (count, value) change = if newValue > 0 && value < 0 || newValue < 0 && value > 0 || newValue == 0 
                                        then (count + currentCount + 1, normalizedNewValue)
                                        else (count + currentCount, normalizedNewValue)
                                       where 
                                        newValue = value + change
                                        normalizedNewValue = newValue `mod` 100
                                        currentCount = abs newValue `div` 100

integrateAll changes = fst $ foldl integrateCount (0, 50) changes

main = do
    content <- readFile "input.txt"
    print . integrateAll . numbers . lines $ content