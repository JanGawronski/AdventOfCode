module Main where
import Data.List (transpose)

inputToLists :: String -> [(String, [Int])]
inputToLists = map (\(op:numbers) -> (op, map read numbers)) . transpose . reverse . map words . lines

operate ("+", numbers) = sum numbers
operate ("*", numbers) = product numbers
operate _ = 0

main = do
    contents <- readFile "input.txt"
    print . sum . map operate . inputToLists $ contents