module Main where

import           System.IO
import           System.Environment
import           AOC
import           Day2
import           Day3
import           Day4
import           Day1
import           Day5
import           Day7

getDay :: Int -> Day
getDay i = [day1, day2, day3, day4, day5, day7, day7] !! (i - 1)

main :: IO ()
main = do
    (d : p : _) <- fmap read <$> getArgs :: IO [Int]
    input       <- getContents
    let res = getDay d
    let f   = if p == 1 then part1 else part2
    putStr $ f res input
