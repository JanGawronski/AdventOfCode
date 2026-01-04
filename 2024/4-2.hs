module Main where
import Data.List (transpose)


searchXmas ('M':t:'M':top) (l:'A':r:mid) ('S':b:'S':bottom) = 1 + searchXmas (t:'M':top) ('A':r:mid) (b:'S':bottom)
searchXmas ('M':t:'S':top) (l:'A':r:mid) ('M':b:'S':bottom) = 1 + searchXmas (t:'S':top) ('A':r:mid) (b:'S':bottom)
searchXmas ('S':t:'M':top) (l:'A':r:mid) ('S':b:'M':bottom) = 1 + searchXmas (t:'M':top) ('A':r:mid) (b:'M':bottom)
searchXmas ('S':t:'S':top) (l:'A':r:mid) ('M':b:'M':bottom) = 1 + searchXmas (t:'S':top) ('A':r:mid) (b:'M':bottom)
searchXmas (t:top) (m:mid) (b:bottom) = searchXmas top mid bottom
searchXmas _ _ _ = 0

search3 f list = sum $ zipWith3 f list (tail list) (tail . tail $ list)

main = do
  contents <- readFile "input.txt"
  print . search3 searchXmas . lines $ contents
