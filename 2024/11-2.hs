module Main where
import Data.Map (fromList, unionsWith, foldrWithKey, singleton, unionWith, empty, foldr)

inputToMap = map (\x -> ((read :: String -> Int) x, 1)) . words

numberSize number = if number < 10 then 1 else 1 + numberSize (number `div` 10)

rules 0 = Left 1
rules x = if even . numberSize $ x then Right (x `div` (10 ^ (numberSize x `div` 2)), x `mod` (10 ^ (numberSize x `div` 2))) else Left (x * 2024)


add (Left x) a = singleton x a
add (Right (x, y)) a = if x == y then singleton x (2 * a) else fromList [(x, a), (y, a)]

blink = foldrWithKey (\k a -> unionWith (+) $ add (rules k) a) empty


main = do
  contents <- readFile "input.txt"
  print . Data.Map.foldr (+) 0 . (!! 75) . iterate blink  . fromList . inputToMap $ contents
  
