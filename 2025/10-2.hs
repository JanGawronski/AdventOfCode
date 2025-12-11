module Main where
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.SBV
import Data.SBV.Control

inputToList :: String -> [([[Int]] ,[Int])]
inputToList = map (\l -> (map (read . map (\x -> if x == '(' then '[' else if x == ')' then ']' else x)) . tail . init . words $ l,
                          read . map (\x -> if x == '{' then '[' else if x == '}' then ']' else x) . last . words $ l))
                        . lines

solveSBV :: [[Int]] -> [Int] -> IO Int64
solveSBV buttons voltages = runSMTWith z3{verbose=False} $ do
  let n = length buttons
      m = length voltages

  xs <- mapM (\i -> sInteger ("x_" ++ show i)) [0 .. n - 1]

  mapM_ (\x -> constrain $ x .>= 0) xs

  forM_ [0 .. m - 1] $ \r -> do
    let covering = [ xs !! j | j <- [0 .. n - 1], r `elem` (buttons !! j) ]
    constrain $ sum covering .== literal (toInteger (voltages !! r))

  let total = sum xs
  query $ do
    let ub = sum (map toInteger voltages)
    let go lo hi
          | lo >= hi  = return lo
          | otherwise = do
              let mid = (lo + hi) `div` 2
              push 1
              constrain $ total .<= literal mid
              cs <- checkSat
              pop 1
              case cs of
                Sat -> go lo mid
                _   -> go (mid + 1) hi
    minTotal <- go 0 ub
    push 1
    constrain $ total .<= literal minTotal
    cs2 <- checkSat
    case cs2 of
      Sat -> do
        vals <- mapM getValue xs
        pop 1
        return (fromIntegral (sum vals) :: Int64)
      _ -> error "Unexpected: no model for minimal total"

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = inputToList contents
    totalsList <- mapM (uncurry solveSBV) parsed
    print (sum totalsList)