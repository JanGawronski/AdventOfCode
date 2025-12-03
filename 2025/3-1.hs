module Main where

inputToListOfLists :: String -> [[Integer]]
inputToListOfLists = map (map (read . (:[]))) . lines

maximum2 list = if endOfTheList then (mx2, mx) else (mx, mx2)
          where 
            mx = maximum list
            (begining, rest) = span (/=mx) list
            endOfTheList = length rest == 1
            mx2 = maximum $ if endOfTheList then begining else tail rest

toDecimal (d1, d2) = d1 * 10 + d2


main = do
    contents <- readFile "input.txt"
    print . sum . map (toDecimal . maximum2) . inputToListOfLists $ contents