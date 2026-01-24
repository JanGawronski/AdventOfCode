module Main where
import Data.Bifunctor (bimap)
import Data.Char (isNumber)
import Data.List (sort, group)

inputToList :: String -> [((Int, Int), (Int, Int))]
inputToList = map (bimap (bimap (read . filter (\x -> isNumber x || x == '-')) (read . filter (\x -> isNumber x || x == '-')) . span (/=',')) (bimap (read . filter (\x -> isNumber x || x == '-')) (read . filter (\x -> isNumber x || x == '-')) . span (/=',')) . span (/=' ')) . lines

move w h steps (p_x, p_y) (v_x, v_y) = ((p_x + v_x * steps) `mod` w, (p_y + v_y * steps) `mod` h)

quadrant w h (x, y) | x < w `div` 2 && y < h `div` 2 = 0
                    | x > w `div` 2 && y < h `div` 2 = 1
                    | x < w `div` 2 && y > h `div` 2 = 2
                    | x > w `div` 2 && y > h `div` 2 = 3
                    | otherwise = 4
main = do
  contents <- readFile "input.txt"
  let width = 101
  let height = 103
  let steps = 100
  print . product . map length . init . group . sort . map (quadrant width height . uncurry (move width height steps)) . inputToList $ contents
