{-# LANGUAGE LambdaCase #-}
module Day2
    ( day2
    )
where

import           AOC
import           Intcode
import           Data.Maybe
import           Data.List
import           Debug.Trace

addOp :: Op
addOp = RWOp 3 $ \[x, y] d -> Set d (x + y)

multOp :: Op
multOp = RWOp 3 $ \[x, y] d -> Set d (x * y)

halt :: Op
halt = ROp 0 $ \[] -> End

spec :: OpSpec
spec = OpSpec $ \case
    1  -> addOp
    2  -> multOp
    99 -> halt

bruteforcePairs :: Int -> [(Int, Int)]
bruteforcePairs n = (,) <$> [0 .. n] <*> [0 .. n]

p2 :: Intcode -> Int
p2 c = 100 * noun + verb
  where
    ff (n, v) = evalProgram spec (setInputs [n, v] c) == 19690720
    (noun, verb) = fromJust $ find ff (bruteforcePairs 99)

day2 = day (evalProgram spec . setInputs [12, 2]) p2
