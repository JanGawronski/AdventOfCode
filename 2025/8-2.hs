module Main where
import Data.List (sortBy, nub)
import Data.Function (on)

coordinate :: [Char] -> (Int, Int, Int)
coordinate = (\(x:y:z:rest) -> (x, y, z)) . map read . words . map (\x -> if x == ',' then ' ' else x)

distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

distances vertices = [((u, v), distance u v) | u <- vertices, v <- vertices, u < v]

insertEdge (u, v) graph = (nub . concat $ ([u, v] : toConnect)) : rest
                    where
                        toConnect = filter (\x -> u `elem` x || v `elem` x) graph 
                        rest = filter (\x -> not (u `elem` x || v `elem` x)) graph 

connectAll graph (edge:edges) = if (length . head $ newGraph) == 1000 then edge else connectAll newGraph edges
                            where
                                newGraph = insertEdge edge graph

main = do
    contents <- readFile "input.txt"
    print . (\((x1, _, _), (x2, _, _)) -> x1 * x2) . connectAll [] . map fst . sortBy (compare `on` snd) . distances . map coordinate . lines $ contents