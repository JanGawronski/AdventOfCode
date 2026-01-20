module Main where

unpackData :: Bool -> Int -> [Char] -> [(Int, Maybe Int)]
unpackData True number (x:list) = (read [x], Just number) : unpackData False (number + 1) list
unpackData False number (x:list) = (read [x], Nothing) : unpackData True number list
unpackData _ _ [] = []

putInEmpty (s, i) ((sl, Just il):list) acc = if i == il then Nothing else putInEmpty (s, i) list ((sl, Just il):acc)

putInEmpty (s, i) ((sl, Nothing):list) acc | s == sl = Just (reverse acc ++ ((s, Just i): map (\x -> if x == (s, Just i) then (s, Nothing) else x) list))
                                                | s >  sl = putInEmpty (s, i) list ((sl, Nothing):acc)
                                                | s <  sl = Just (reverse acc ++ ((s, Just i):(sl - s, Nothing): map (\x -> if x == (s, Just i) then (s, Nothing) else x) list))

combineNothing ((x, Nothing):(y, Nothing):list) = combineNothing ((x + y, Nothing):list)
combineNothing (x:list) = x : combineNothing list
combineNothing [] = []

packData list ((_, Nothing):rev) = packData list rev
packData list ((sr, Just i):rev) = case putInEmpty (sr, i) list [] of
                                        Nothing -> packData list rev
                                        Just x -> packData (combineNothing x) rev
packData list rev = list

unpackOnceAgain ((s, Just i):list) = replicate s i ++ unpackOnceAgain list
unpackOnceAgain ((s, Nothing):list) = replicate s 0 ++ unpackOnceAgain list
unpackOnceAgain [] = []


main = do
  contents <- readFile "input.txt"
  let disk = unpackData True 0 $ init contents
  print . sum . zipWith (*) [0..] . unpackOnceAgain $ packData disk (reverse disk)
