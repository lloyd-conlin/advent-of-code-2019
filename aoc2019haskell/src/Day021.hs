module Day021 where

import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

type Opcode = Int
type Addr = Int
type Memory = Map.Map Addr Int

data Err = Err deriving Show

day021 = do
    contents <- readFile "test/raw-input2.txt"
    print $ flip executeInstr 0 $ parseProgram $ map readInt $ splitOn "," contents

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
            _  -> Left Err
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
            )))

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
            )))

lookupOperand :: Memory -> Addr -> Either Err Int
lookupOperand mem addr = case Map.lookup addr mem of
    Just x  -> Right x
    Nothing -> Left Err

nVList :: [(Int, Int)]
nVList = [(n, v) | n <- [0..99], v <- [0..99]]
