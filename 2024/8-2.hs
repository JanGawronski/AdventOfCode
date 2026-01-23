module Main where
import Data.List (groupBy, sort, nub)
import Data.Function (on)

inputToList = concatMap (filter ((/= '.') . fst)) . zipWith (\y row -> zipWith (\x e -> (e, (x, y))) [1..] row) [1..] . lines

groupList = map (map snd) . groupBy ((==) `on` fst) . sort

antinode (x1, y1) (x2, y2) h w = zip [x1, x1 + xdif .. xend] [y1, y1 + ydif .. yend]
                                 where
                                   g = gcd (abs (x2 - x1)) (abs (y2 - y1))
                                   xdif = (x2 - x1) `div` g
                                   ydif = (y2 - y1) `div` g
                                   xend = if x2 >= x1 then w else 1
                                   yend = if y2 >= y1 then h else 1
                                   
antinodes h w list = concat [antinode x y h w | x <- list, y <- list, x /= y]

main = do
  contents <- readFile "input.txt"
  let width = length . head . lines $ contents
  let height = length . lines $ contents
  print . length . nub . concatMap (antinodes height width) . groupList . inputToList $ contents

