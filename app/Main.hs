module Main where

import           System.IO
import           System.Environment
import           AOC
import           Day1                          as D1
import           Day2                          as D2
import           Day3                          as D3
import           Day4                          as D4

getDay :: Int -> Day
getDay 1 = D1.day
getDay 2 = D2.day
getDay 3 = D3.day
getDay 4 = D4.day
getDay _ = undefined

main :: IO ()
main = do
    (d : p : _) <- fmap read <$> getArgs :: IO [Int]
    input       <- getContents
    let res = getDay d
    let f   = if p == 1 then part1 else part2
    print $ f res input
