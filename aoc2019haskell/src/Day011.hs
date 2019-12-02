module Day011 where

import System.IO
import Control.Monad

-- main :: IO ()
day011 = do
    contents <- readFile "test/raw-input1.txt"
    print $ sum $ map fuelCost $ map readInt $ words contents


readInt :: String -> Int
readInt = read

fuelCost :: Int -> Int
fuelCost mass = (mass `div` 3) - 2
