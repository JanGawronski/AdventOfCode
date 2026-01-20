module Main where
import Data.ByteString (unpack)


unpackData True number (x:list) = replicate (read [x]) (Just number) ++ unpackData False (number + 1) list
unpackData False number (x:list) = replicate (read [x]) Nothing ++ unpackData True number list
unpackData _ _ [] = []

packData list (Nothing:rev) = packData list rev
packData (Nothing:list) (Just x:rev) = x : packData list rev
packData (Just x:list) rev = x : packData list rev

main = do
  contents <- readFile "input.txt"
  let disk = unpackData True 0 . init $ contents
  print . sum . zipWith (*) [0..] . take (length $ filter (/=Nothing) disk) $ packData disk (reverse disk)
