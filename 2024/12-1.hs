module Main where
import Data.Map (fromList, lookup, delete, insert, keys, null)
import Data.Set (singleton, union, null, empty, member, toList, size)
import Data.Bifunctor (second)

inputToMap = fromList . concat . zipWith (\y row -> zipWith (\x e -> ((x, y), e) ) [1..] row) [1..] . lines

gatherGroup m (x, y) = foldr (\c (accM, accR) -> if Data.Map.lookup c accM == Data.Map.lookup (x, y) m then second (union accR) $ gatherGroup accM c else (accM, accR)) (delete (x, y) m, singleton (x, y)) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

gatherAllGroups (m, s) | Data.Set.null s = gatherAllGroups $ gatherGroup m (head . keys $ m)
                       | Data.Map.null m = [s]
                       | otherwise = s : gatherAllGroups (gatherGroup m (head . keys $ m))



perimeter s (x, y) = (4-) . length . filter (`member` s) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

perimeterGroup s = sum . map (perimeter s) $ toList s


main = do
  contents <- readFile "input.txt"
  print . sum . map (\s -> perimeterGroup s * size s) . flip (curry gatherAllGroups) empty . inputToMap $ contents
