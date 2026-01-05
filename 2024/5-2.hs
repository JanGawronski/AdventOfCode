module Main where
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (intersect, nub)

splitOn acc ('\n':'\n':text) = (reverse acc, text)
splitOn acc (x:text) = splitOn (x:acc) text

inputToData :: [Char] -> ([(Int, Int)], [[Int]])
inputToData = bimap (map (bimap read (read . tail) . span (/='|')) . lines) (map (map read . words . map (\x -> if x == ',' then ' ' else x)) . lines) . splitOn ""

violatesRule (x:ordering) (first, second) | first == x = False
                                       | second == x = first `elem` ordering
                                       | otherwise = violatesRule ordering (first, second)
violatesRule _ _ = False

correct rules ordering = not $ any (violatesRule ordering) rules

successes vertex = map fst . filter ((==vertex) . snd)

topological edges vertex order = if vertex `elem` order then order
                                   else vertex : foldr (topological edges) order (successes vertex edges)

correctOrdering rules incorrectOrdering = reverse $ foldr (topological filteredRules) [] (filter (\x-> not $ any ((==x) . fst) filteredRules) incorrectOrdering)
                                         where
                                           filteredRules = filter (\(x, y) -> x `elem` incorrectOrdering && y `elem` incorrectOrdering) rules
                                           
main = do
  contents <- readFile "input.txt"
  let (rules, orderings) = inputToData contents
  print . sum . map ((\x -> x !! (length x `div` 2)) . correctOrdering rules) . filter (not . correct rules) $ orderings
