module Main where
import Data.Bifunctor (bimap)

divideByDoubleNewline acc ('\n':'\n':list) = reverse acc : divideByDoubleNewline [] list
divideByDoubleNewline acc (x:list) = divideByDoubleNewline (x:acc) list
divideByDoubleNewline acc [] = [reverse acc] 

parsePatterns = map $ length . filter (=='#')

parseSizes :: String -> [((Int, Int), [Int])]
parseSizes = map (bimap (bimap read (read . tail) . span (/='x')) (map read . words . tail) . span (/=':')) . lines

compareSizes patterns = length . filter (\((w, h), y) -> w * h > sum (zipWith (*) patterns y))

main = do
    contents <- readFile "input.txt"
    let parsed = divideByDoubleNewline [] contents
    let patterns = parsePatterns $ init parsed
    let sizes = parseSizes $ last parsed
    print $ compareSizes patterns sizes