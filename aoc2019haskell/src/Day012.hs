module Day012 where

import System.IO
import Control.Monad

-- main :: IO ()
day012 = do
    contents <- readFile "test/raw-input1.txt"
    print $ sum $ map fuelMass $ map fuelCost $ map readInt $ words contents


readInt :: String -> Int
readInt = read

fuelCost :: Int -> Int
fuelCost mass = (mass `div` 3) - 2

fuelMass :: Int -> Int
fuelMass mass = fuelMass' mass mass

fuelMass' :: Int -> Int -> Int
fuelMass' mass acc
    | fuelCost mass <= 0 = acc
    | otherwise          = fuelMass' (fuelCost mass) $ acc + fuelCost mass
