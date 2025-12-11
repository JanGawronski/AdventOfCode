module Main where
import Data.Map (fromList, member, (!), insert)
import Data.Bifunctor (second, first)

inputToMap = fromList . map (second (words . tail) . span (/=':')) . lines

countPaths vertex mem graph = if vertex `member` mem
      then (mem ! vertex, mem)
      else (\(t, m) -> (t, insert vertex t m)) 
            . (if vertex == "fft" then first (\(t1,t2,t3,t4) -> (t1 + t3, t2 + t4, 0, 0)) else if vertex == "dac" then first (\(t1,t2,t3,t4) -> (t1 + t2, 0, t3 + t4, 0)) else id)
            $ foldl (\((acc1, acc2, acc3, acc4), m) c -> first (\(t1,t2,t3,t4) -> (acc1 + t1, acc2 + t2, acc3 + t3, acc4 + t4)) 
            $ countPaths c m graph) ((0,0,0,0), mem) (graph ! vertex)

main = do
    contents <- readFile "input.txt"
    print . (\(x,_,_,_) -> x) . fst . countPaths "svr" (fromList [("out", (0, 0, 0, 1))]) . inputToMap $ contents