module Day022 where

import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

type Addr = Int
type Memory = Map.Map Addr Int

data Err = LookupErr | InvalidOp Int Int deriving Show

day022 = do
    contents <- readFile "test/raw-input2.txt"
    let original = (parseProgram $ map readInt $ splitOn "," contents)
    print $ evalProg original original nVList

modifyMemory :: (Int, Int) -> Memory -> Memory
modifyMemory (n, v) mem = Map.insert 1 n $ Map.insert 2 v mem

evalProg :: Memory -> Memory -> [(Int, Int)] -> Either Err (Int, Int)
evalProg original mem nVs = (executeInstr (modifyMemory x original) 0) >>= (\x -> checkResult x original nVs)
    where x = head nVs

checkResult :: Memory -> Memory -> [(Int, Int)] -> Either Err (Int, Int)
checkResult mem original (x:nVs)
    | Map.lookup 0 mem == (Just 19690720) = Right x
    | otherwise                           = evalProg original mem nVs

readInt :: String -> Int
readInt = read

parseProgram :: [Int] -> Memory
parseProgram prog = Map.fromList $ zip [0..] prog

executeInstr :: Memory -> Addr -> Either Err Memory
executeInstr mem start =
    (lookupOperand mem start) >>=
        (\y -> case y of
            1  -> (executeAdd mem $ start + 1) >>= (\x -> executeInstr x $ start + 4)
            2  -> (executeMul mem $ start + 1) >>= (\x -> executeInstr x $ start + 4)
            99 -> Right mem
            x  -> Left (InvalidOp x start)
        )

executeAdd :: Memory -> Addr -> Either Err Memory
executeAdd mem operand =
    (lookupOperand mem operand >>= (\a -> lookupOperand mem a)) >>=
        (\x -> (lookupOperand mem (operand + 1) >>= (\b -> lookupOperand mem b)) >>=
            (\y -> (lookupOperand mem $ operand + 2) >>=
                (\z ->
                    Right (Map.insert
                        z
                        (x + y)
                        mem
                    )
                )
            )
        )

executeMul :: Memory -> Addr -> Either Err Memory
executeMul mem operand =
    (lookupOperand mem operand >>= (\a -> lookupOperand mem a)) >>=
        (\x -> (lookupOperand mem (operand + 1) >>= (\b -> lookupOperand mem b)) >>=
            (\y -> (lookupOperand mem $ operand + 2) >>=
                (\z ->
                    Right (Map.insert
                        z
                        (x * y)
                        mem
                    )
                )
            )
        )

lookupOperand :: Memory -> Addr -> Either Err Int
lookupOperand mem addr = case Map.lookup addr mem of
    Just x  -> Right x
    Nothing -> Left LookupErr

nVList :: [(Int, Int)]
nVList = [(n, v) | n <- [0..99], v <- [0..99]]
