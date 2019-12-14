{-# LANGUAGE LambdaCase #-}
module Day5
    ( day5
    , spec
    )
where


import           Data.Maybe
import           Intcode
import           AOC
import           Debug.Trace


spec :: OpSpec
spec = OpSpec $ \case
    -- ADD (x y) -> d
    1  -> RWOp 3 $ \[x, y] d -> Set d (x + y)
    -- MUL (x y) -> d
    2  -> RWOp 3 $ \[x, y] d -> Set d (x * y)
    -- GET input
    3  -> WOp 1 $ \d -> Input d
    -- SET output
    4  -> ROp 1 $ \[x] -> Output x
    -- JMP true
    5  -> ROp 2 $ \[x, y] -> if x /= 0 then Jump y else Continue
    -- JMP false
    6  -> ROp 2 $ \[x, y] -> if x == 0 then Jump y else Continue
    -- LE
    7  -> RWOp 3 $ \[x, y] d -> Set d (if x < y then 1 else 0)
    -- EQ
    8  -> RWOp 3 $ \[x, y] d -> Set d (if x == y then 1 else 0)
    -- HALT
    99 -> ROp 0 $ \[] -> End

day5 =
    let part1 ic = fromJust . evalProgram spec ic $ (initialize >> resume 1)
        part2 ic = fromJust . evalProgram spec ic $ (initialize >> resume 5)
    in  day part1 part2
