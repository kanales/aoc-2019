{-# LANGUAGE LambdaCase #-}
module Day2
    ( day2
    )
where

import           Data.List
import           Data.Maybe

import           AOC
import           Intcode

import           Debug.Trace

trace' :: (Show x) => x -> x
trace' x = trace (show x) x

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

eval :: OpSpec -> Intcode -> Int
eval spec ic = head . getIntcode $ execProgram spec ic initialize

p2 :: Intcode -> Int
p2 c =
    let ff (n, v) = eval spec (setInputs [n, v] c) == 19690720
        (noun, verb) = fromJust $ find ff (bruteforcePairs 99)
    in  100 * noun + verb


setInputs :: [Int] -> Intcode -> Intcode
setInputs inputs =
    let f c = head c : inputs ++ drop 3 c in Intcode . f . getIntcode

day2 = let part1 ic = eval spec (setInputs [12, 2] ic) in day part1 p2
