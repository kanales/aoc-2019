module Day1
    ( day1
    )
where

import           AOC
import           Parser

newtype Input = Input { fromInput :: [Int] }

instance Read Input where
    readsPrec _ = readP_to_S $ Input <$> sepBy integer (char '\n')


getFuel :: Int -> Int
getFuel x = x `div` 3 - 2

p1 :: Input -> Int
p1 = sum . fmap getFuel . fromInput

p2 :: Input -> Int
p2 = sum . fmap fun . fromInput
    where fun = sum . tail . takeWhile (> 0) . iterate getFuel

day1 = day p1 p2
