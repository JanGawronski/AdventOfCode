module Main where
import Data.List (groupBy)
import Data.Char (isNumber)
import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)

inputToList = map ((\[a, b, p] -> (a, b, p)) . map (bimap (read . filter isNumber) (read . filter isNumber) . span (/=','))) . filter (/=[""]) . groupBy (\a b -> a /= "" && b /= "") . lines

calculate ((a_x, a_y), (b_x, b_y), (x, y)) = ((x `div` a_x) - (y * a_x - x * a_y) * b_x `div` (b_y * a_x - b_x * a_y) `div` a_x, (y * a_x - x * a_y) `div` (b_y * a_x - b_x * a_y))

isCorrect ((a_x, a_y), (b_x, b_y), (x, y)) (a, b) = x == a * a_x + b * b_x && y == a * a_y + b * b_y

correctCalculate numbers = if isCorrect numbers (calculate numbers) then Just (calculate numbers) else Nothing

main = do
  contents <- readFile "input.txt"
  print . sum . map (\(Just(a, b)) -> 3 * a + b) . filter (/=Nothing) . map correctCalculate . inputToList $ contents
