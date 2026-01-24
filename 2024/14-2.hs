module Main where
import Data.Bifunctor (bimap)
import Data.Char (isNumber)
import Data.List (sort, group, genericLength, transpose, minimumBy)
import Data.Function (on)

inputToList :: String -> [((Int, Int), (Int, Int))]
inputToList = map (bimap (bimap (read . filter (\x -> isNumber x || x == '-')) (read . filter (\x -> isNumber x || x == '-')) . span (/=',')) (bimap (read . filter (\x -> isNumber x || x == '-')) (read . filter (\x -> isNumber x || x == '-')) . span (/=',')) . span (/=' ')) . lines

move w h (v_x, v_y) (p_x, p_y) = ((p_x + v_x) `mod` w, (p_y + v_y) `mod` h)

mean x = sum x `div` length x

variance x = mean (map (^2) x) - (^2) (mean x)

main = do
  contents <- readFile "input.txt"
  let width = 101
  let height = 103
  print . (10000-) . length . dropWhile (\(x, y) -> x > 500 || y > 500) . map (bimap variance variance . unzip) . transpose . map (\(p, v) -> take 10000 $ iterate (move width height v) p) $ inputToList contents
