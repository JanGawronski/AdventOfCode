module Main where
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.Ord (comparing, Down(..))

coordinate :: [Char] -> (Int, Int, Int)
coordinate = (\(x:y:z:rest) -> (x, y, z)) . map read . words . map (\x -> if x == ',' then ' ' else x)

distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

distances vertices = [((u, v), distance u v) | u <- vertices, v <- vertices, u < v]

topn n = take n . sortBy (compare `on` snd)

insertEdge (u, v) graph = (nub . concat $ ([u, v] : toConnect)) : rest
                    where
                        toConnect = filter (\x -> u `elem` x || v `elem` x) graph 
                        rest = filter (\x -> not (u `elem` x || v `elem` x)) graph 

main = do
    contents <- readFile "input.txt"
    print . product . take 3 . sortBy (comparing Down) . map length . foldr (insertEdge . fst) [] . topn 1000 . distances . map coordinate . lines $ contents
