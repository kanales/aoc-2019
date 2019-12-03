module Day1
    ( day
    )
where

import           AOC


parseInput :: String -> [Int]
parseInput = fmap read . lines

getFuel :: Int -> Int
getFuel x = x `div` 3 - 2

p1 :: [Int] -> Int
p1 = sum . fmap getFuel


p2 :: [Int] -> Int
p2 = sum . fmap fun where fun = sum . tail . takeWhile (> 0) . iterate getFuel

day = Day { part1 = p1 . parseInput, part2 = p2 . parseInput }
