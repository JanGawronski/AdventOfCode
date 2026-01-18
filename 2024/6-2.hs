module Main where
import Data.List (nub, (\\))
import Data.Set (fromList, member, insert, notMember)

obstacles = concat . zipWith (\y row -> filter (/= (0, 0)) $ zipWith (\x e -> if e == '#' then (x, y) else (0, 0)) [1..] row) [1..]

guard = head . concat . zipWith (\y row -> filter (/= (0, 0, ' ')) $ zipWith (\x e -> if e == '^' then (x, y, e) else (0, 0, ' ')) [1..] row) [1..]

isCycle height width obstacles (x, y, direction) path | nextX <= 0 || nextY <= 0 || nextX > width || nextY > height = False
                                                     | (nextX, nextY, nextDirection) `member` path = True
                                                     | otherwise = isCycle height width obstacles (nextX, nextY, nextDirection) ((x, y, direction) `insert` path) 
                                          where
                                          ((moveX, moveY), rotateDirection) = case direction of
                                                                       '^' -> ((x, y - 1), '>')
                                                                       '>' -> ((x + 1, y), 'v')
                                                                       'v' -> ((x, y + 1), '<')
                                                                       '<' -> ((x - 1, y), '^')
                                          (nextX, nextY, nextDirection) = if (moveX, moveY) `member` obstacles then (x, y, rotateDirection) else (moveX, moveY, direction)


move height width obstacles ((x, y, direction):path) = if nextX <= 0 || nextY <= 0 || nextX > width || nextY > height || (nextX, nextY, nextDirection) `elem` path then (x, y, direction):path
                                                       else move height width obstacles ((nextX, nextY, nextDirection):(x, y, direction):path) 
                                          where
                                          ((moveX, moveY), rotateDirection) = case direction of
                                                                       '^' -> ((x, y - 1), '>')
                                                                       '>' -> ((x + 1, y), 'v')
                                                                       'v' -> ((x, y + 1), '<')
                                                                       '<' -> ((x - 1, y), '^')
                                          (nextX, nextY, nextDirection) = if (moveX, moveY) `member` obstacles then (x, y, rotateDirection) else (moveX, moveY, direction)
 

main = do
  contents <- readFile "input.txt"
  let grid = lines contents
  let h = length grid
  let w = length . head $ grid
  let obs = fromList . obstacles $ grid
  let first = guard grid
  let obstaclesToAdd = nub . map (\(x, y, _) -> (x, y)) $ move h w obs [first]
  print $ length . filter id . map (\o -> isCycle h w (o `insert` obs) first (fromList [])) $ obstaclesToAdd
