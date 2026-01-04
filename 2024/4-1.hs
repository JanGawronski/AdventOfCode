module Main where
import Data.List (transpose)

occurences _ [] = 0
occurences substring string = (if take (length substring) string == substring then 1 else 0) + occurences substring (tail string)

diagonal = zipWith (\x y -> replicate x '.' ++ y) [0..]

countWord word list2d = sum . map (occurences word) $ list2d
                                                   ++ map reverse list2d
                                                   ++ transpose list2d
                                                   ++ map reverse (transpose list2d)
                                                   ++ transpose (diagonal list2d)
                                                   ++ transpose (diagonal (map reverse list2d))
                                                   ++ transpose (diagonal (reverse list2d))
                                                   ++ transpose (diagonal (reverse (map reverse list2d)))

main = do
  contents <- readFile "input.txt"
  print . countWord "XMAS" . lines $ contents
