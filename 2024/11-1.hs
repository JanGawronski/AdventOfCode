module Main where

numberSize number = if number < 10 then 1 else 1 + numberSize (number `div` 10)

rules 0 _ = 1
rules d 0 = rules (d - 1) 1
rules d x = if even . numberSize $ x then rules (d - 1) (x `div` (10 ^ (numberSize x `div` 2))) + rules (d - 1) (x `mod` (10 ^ (numberSize x `div` 2))) else rules (d - 1) (x * 2024)

main = do
  contents <- readFile "input.txt"
  print . sum . map (rules 25 . read) . words $ contents
