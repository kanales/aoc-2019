{-# LANGUAGE LambdaCase #-}
module Day5
    ( day5
    )
where

import qualified Data.Vector.Mutable           as VM
import           Control.Monad.ST
import           Intcode
import           AOC
import           Debug.Trace

addOp :: Op
addOp = RWOp 3 $ \[x, y] d -> Set d (x + y)

multOp :: Op
multOp = RWOp 3 $ \[x, y] d -> Set d (x * y)

halt :: Op
halt = ROp 0 $ \[] -> End

inOp :: Int -> Op
inOp x = WOp 1 $ \d -> Set d x

outOp :: Op
outOp = ROp 1 $ \[x] -> Output x


jumpTrueOp :: Op
jumpTrueOp = RWOp 2 $ \[x] d -> if x /= 0 then Jump d else Continue

jumpFalseOp :: Op
jumpFalseOp = RWOp 2 $ \[x] d -> if x == 0 then Jump d else Continue


lessOp :: Op
lessOp = RWOp 3 $ \[x, y] d -> Set d (if x < y then 1 else 0)

eqOp :: Op
eqOp = RWOp 3 $ \[x, y] d -> Set d (if x == y then 1 else 0)

spec :: Int -> OpSpec
spec i = OpSpec $ \x -> case trace (show x) x of
    1  -> addOp
    2  -> multOp
    3  -> inOp i
    4  -> outOp
    5  -> jumpTrueOp
    6  -> jumpFalseOp
    7  -> lessOp
    8  -> eqOp
    99 -> halt

day5 = day (evalProgram $ spec 1) (evalProgram $ spec 5)
