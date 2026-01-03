module Main where
import Data.Bifunctor (bimap)
import Data.List (transpose, sort, group)

inputTolist :: String -> [(Int, Int)]
inputTolist = map (bimap read read . span (/= ' ')) . lines

sortTupleList = (\(x:y:_) -> (x, y)) . map sort . transpose . map (\(x, y) -> [x, y])

groupCount = map (\x -> (head x, length x)) . group

similarityScore ((lnum, lcount):left) ((rnum, rcount):right) | lnum == rnum = lnum * lcount * rcount + similarityScore left right
                                                             | lnum > rnum = similarityScore ((lnum, lcount):left) right
                                                             | otherwise = similarityScore left ((rnum, rcount):right)
similarityScore _ _ = 0
                                                             

main = do
  contents <- readFile "input.txt"
  print . uncurry similarityScore . bimap groupCount groupCount . sortTupleList . inputTolist $ contents

