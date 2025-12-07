module Main where
import Data.IntMap.Lazy (fromList, intersection, mapKeys, difference, unionWith, elems)
import Data.Bifunctor (second)

stringToMap = fromList . map (second $ const 1) . filter ((/='.') . snd) . zip [1..]

propagate beamMap splitterMap = unionWith (+) left . unionWith (+) right $ difference beamMap int
                            where
                                int = intersection beamMap splitterMap
                                right = mapKeys succ int
                                left = mapKeys pred int

main = do
    contents <- readFile "input.txt"
    let (beamMap:splitterMaps) = map stringToMap . lines $ contents
    print . sum . elems $ foldl propagate beamMap splitterMaps