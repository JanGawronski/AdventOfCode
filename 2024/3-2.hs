module Main where
import Data.Char (isDigit)

matchMul ('m':'u':'l':'(':list) = matchFirstNumber list + matchMul list
matchMul ('d':'o':'n':'\'':'t':'(':')':list) = matchDo list
matchMul (_:list) = matchMul list
matchMul [] = 0

matchDo ('d':'o':'(':')':list) = matchMul list
matchDo (_:list) = matchDo list
matchDo [] = 0

matchNumber :: Int -> [Char] -> (Int, [Char])
matchNumber n list = (read number, drop (length number) list)
                   where number = takeWhile isDigit $ take n list

matchFirstNumber list = number * matchComma rest
                      where (number, rest) = matchNumber 3 list

matchComma (',':list) = matchSecondNumber list
matchComma _ = 0

matchSecondNumber list = number * matchBracket rest
                  where (number, rest) = matchNumber 3 list

matchBracket (')':list) = 1
matchBracket _ = 0

main = do
  contents <- readFile "input.txt"
  print . matchMul $ contents
