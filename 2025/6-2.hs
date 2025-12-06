module Main where
import Data.List (transpose)

columnsWidths (' ':operators) (x:acc) = columnsWidths operators (succ x:acc)
columnsWidths (_:operators) acc = columnsWidths operators (0:acc)
columnsWidths _ (x:acc) = reverse (succ x:acc)

takeList (n:widths) list = take n list : takeList widths (drop (succ n) list)
takeList _ _ = []

rotate :: [Int] -> [String] -> [[Int]]
rotate colWidths = map (map read . transpose) . transpose . map (takeList colWidths)

operate "+" numbers = sum numbers
operate "*" numbers = product numbers
operate _  _ = 0

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let opLine = last input
    let numberLines = init input
    let colWidths = columnsWidths opLine []
    let numbers = rotate colWidths numberLines
    let operators = words opLine
    print . sum $ zipWith operate operators numbers