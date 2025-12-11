module Main where
import Data.Map (fromList, member, (!), insert)
import Data.Bifunctor (second, first)

inputToMap = fromList . map (second (words . tail) . span (/=':')) . lines

countPaths vertex mem graph = if vertex `member` mem
      then (mem ! vertex, mem)
      else (\(t, m) -> (t, insert vertex t m)) $ foldl (\(acc, m) c -> first (+acc) $ countPaths c m graph) (0, mem) (graph ! vertex)

main = do
    contents <- readFile "input.txt"
    print . fst . countPaths "you" (fromList [("out", 1)]) . inputToMap $ contents