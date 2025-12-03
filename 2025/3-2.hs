module Main where

inputToListOfLists = map (map (read . (:[]))) . lines

maximumn 0 _ = []
maximumn n list = if length list <= n then list 
                  else mxb : maximumn (n - 1) (tail $ dropWhile (/= mxb) list)
                where 
                  beginning = take (length list - n + 1) list
                  mxb = maximum beginning

toDecimal [] = 0
toDecimal (h:t) = (h * (10 ^ length t)) + toDecimal t

main = do
    contents <- readFile "input.txt"
    print . sum . map (toDecimal . maximumn 12) . inputToListOfLists $ contents
    