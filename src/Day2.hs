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
import           Control.Monad.Reader

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
p2 c =
    let ff (n, v) =
                runReader (evalProgram spec (setInputs [n, v] c)) 0 == 19690720
        (noun, verb) = fromJust $ find ff (bruteforcePairs 99)
    in  100 * noun + verb


day2 =
    let part1 ic = runReader (evalProgram spec $ setInputs [12, 2] ic) 0
    in  day part1 p2
