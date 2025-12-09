module Main where
import Data.Bifunctor (bimap)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map.Strict (fromList, toList, (!), member)
import qualified Data.Set (fromList, member, insert, size, toList)
import Data.List (nub, sort)

inputToList :: String -> [(Int, Int)]
inputToList = map (bimap read (read . tail) . span (/= ',')) . lines 

rectangleArea (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

maxRectangleArea filled decompMap points = maximumBy (compare `on` snd) [((a, b), rectangleArea (decompMap ! a) (decompMap ! b)) | a <- points, b <- points, inFilled (a, b) filled]

compress points = fromList $ zip points $ map (bimap (x !) (y !)) points
                where
                    x = fromList $ zip (nub . sort . map fst $ points) [1..]
                    y = fromList $ zip (nub . sort . map snd $ points) [1..]

boundaries ((x1, y1):(x2, y2):points) =    (if x1 <= x2 then map (, y1) [x1..x2] else map (, y1) [x2..x1]) 
                                        ++ (if y1 <= y2 then map (x1, ) [y1..y2] else map (x1, ) [y2..y1]) 
                                        ++ boundaries ((x2, y2):points)
boundaries _ = []

inFilled ((x1, y1), (x2, y2)) filled = all (`Data.Set.member` filled) $ boundaries [(x1, y1), (x1, y2), (x2, y2), (x2, y1), (x1, y1)]

neighbours (x, y) set = filter (\x -> not (x `Data.Set.member` set)) [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

fill v set = foldr fill (Data.Set.insert v set) (neighbours v set)

gridPrint set = concat [[if (x, y) `Data.Set.member` set then '#' else '.' | x <- [minx..maxx]] ++ "\n" | y <- [miny..maxy]]
                where
                    maxx = maximum . map fst . Data.Set.toList $ set
                    minx = minimum . map fst . Data.Set.toList $ set
                    maxy = maximum . map snd . Data.Set.toList $ set
                    miny = minimum . map snd . Data.Set.toList $ set

main = do
    contents <- readFile "input.txt"
    let points = inputToList contents
    let compressMap = compress points
    let decompressMap = fromList $ map (\(a,b) -> (b,a)) $ toList compressMap
    let compressedPoints = map (compressMap !) points
    let bounds = Data.Set.fromList . boundaries $ (compressedPoints ++ [head compressedPoints]) 
    let filled = fill (110, 110) bounds
    print $ maxRectangleArea filled decompressMap compressedPoints