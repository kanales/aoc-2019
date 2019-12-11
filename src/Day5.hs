{-# LANGUAGE LambdaCase #-}
module Day5
    ( day5
    )
where

import qualified Data.Vector.Mutable           as VM
import           Control.Monad.ST
import           Control.Monad.Reader
import           Intcode
import           AOC
import           Debug.Trace

addOp :: Op
addOp = RWOp 3 $ \[x, y] d -> Set d (x + y)

multOp :: Op
multOp = RWOp 3 $ \[x, y] d -> Set d (x * y)

halt :: Op
halt = ROp 0 $ \[] -> End

inOp :: Op
inOp = WOp 1 $ \d -> Input d

outOp :: Op
outOp = ROp 1 $ \[x] -> Output x

jumpTrueOp :: Op
jumpTrueOp = ROp 2 $ \[x, y] -> if x /= 0 then Jump y else Continue

jumpFalseOp :: Op
jumpFalseOp = ROp 2 $ \[x, y] -> if x == 0 then Jump y else Continue

lessOp :: Op
lessOp = RWOp 3 $ \[x, y] d -> Set d (if x < y then 1 else 0)

eqOp :: Op
eqOp = RWOp 3 $ \[x, y] d -> Set d (if x == y then 1 else 0)

spec :: OpSpec
spec = OpSpec $ \case
    1  -> addOp
    2  -> multOp
    3  -> inOp
    4  -> outOp
    5  -> jumpTrueOp
    6  -> jumpFalseOp
    7  -> lessOp
    8  -> eqOp
    99 -> halt

day5 =
    let part1 ic = runReader (evalProgram spec ic) 1
        part2 ic = runReader (evalProgram spec ic) 5
    in  day part1 part2
