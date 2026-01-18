module Main where
import Data.List (nub)

obstacles = concat . zipWith (\y row -> filter (/= (0, 0)) $ zipWith (\x e -> if e == '#' then (x, y) else (0, 0)) [1..] row) [1..]

guard = head . concat . zipWith (\y row -> filter (/= (0, 0, ' ')) $ zipWith (\x e -> if e == '^' then (x, y, e) else (0, 0, ' ')) [1..] row) [1..]

move height width obstacles ((x, y, direction):path) = if nextX <= 0 || nextY <= 0 || nextX > width || nextY > height || (nextX, nextY, nextDirection) `elem` path then (x, y, direction):path
                                                       else move height width obstacles ((nextX, nextY, nextDirection):(x, y, direction):path) 
                                          where
                                          ((moveX, moveY), rotateDirection) = case direction of
                                                                       '^' -> ((x, y - 1), '>')
                                                                       '>' -> ((x + 1, y), 'v')
                                                                       'v' -> ((x, y + 1), '<')
                                                                       '<' -> ((x - 1, y), '^')
                                          (nextX, nextY, nextDirection) = if (moveX, moveY) `elem` obstacles then (x, y, rotateDirection) else (moveX, moveY, direction)
                                                                       

main = do
  contents <- readFile "input.txt"
  let grid = lines contents
  print . length . nub . map (\(x, y, _) -> (x, y)) $ move (length grid) (length . head $ grid) (obstacles grid) [guard grid]
