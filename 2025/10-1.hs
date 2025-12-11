module Main where

inputToList :: String -> [([Bool], [[Int]])]
inputToList = map (\l -> (map (== '#') . tail . init . head . words $ l,
                          map (read . map (\x -> if x == '(' then '[' else if x == ')' then ']' else x)) . tail . init . words $ l))
                        . lines

press lights button = zipWith (\i l -> (i `elem` button ) /= l) [0..] lights

check lights (button:buttons) = min (check lights buttons) (succ $ check (press lights button) buttons)
check lights [] = if or lights then 1000000000 else 0

main = do
    contents <- readFile "input.txt"
    print . sum . map (uncurry check) . inputToList $ contents
