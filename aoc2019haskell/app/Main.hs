module Main where

import Lib
import System.Environment
import System.IO

import Day011
import Day012
import Day021
import Day022

solutions :: [IO ()]
solutions = [day011, day012, day021, day022]

findSolution :: Int -> Int -> Maybe (IO ())
findSolution i p
    | 2 * i <= length solutions = Just (solutions !! (2 * (i - 1) + (p - 1)))
    | otherwise              = Nothing

main :: IO ()
main = do
    (day:part:_) <- getArgs
    case findSolution (read day) (read part) of
        Nothing -> putStrLn "No solution for that day."
        Just sol-> sol
