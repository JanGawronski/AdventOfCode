module Main where
import Data.IntSet (fromList, intersection, difference, union, size)
import qualified Data.IntSet (map)

stringToMap = fromList . map fst . filter ((/='.') . snd) . zip [1..]

propagate (beamMap, count) splitterMap = (union left . union right $ difference beamMap int, count + size int)
                            where
                                int = intersection beamMap splitterMap
                                right = Data.IntSet.map succ int
                                left = Data.IntSet.map pred int

main = do
    contents <- readFile "input.txt"
    let (beamMap:splitterMaps) = map stringToMap . lines $ contents
    print . snd $ foldl propagate (beamMap, 0) splitterMaps