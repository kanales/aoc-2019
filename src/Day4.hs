module Day4
    ( day4
    )
where

import           AOC
import           Data.List
import           Data.Char
import           Data.List.Split
import           Control.Applicative
import           Parser


data Range = Range { lower :: Int, upper :: Int }

instance Show Range where
    show r = (show $ lower r) ++ "-" ++ (show $ upper r)

instance Read Range where
    readsPrec _ = readP_to_S $ Range <$> integer <* char '-' <*> integer

isPassword :: Int -> Bool
isPassword is =
    let digits = show is
        repeat = or $ zipWith (==) digits (tail digits)
        order  = and $ zipWith (<=) digits (tail digits)
    in  repeat && order

isPassword' :: Int -> Bool
isPassword' is =
    let digits = show is
        repeat = elem 2 . fmap length $ group digits
        order  = and $ zipWith (<=) digits (tail digits)
    in  repeat && order

bruteForce :: (Int -> Bool) -> Range -> [Int]
bruteForce f (Range l h) = filter f [l .. h]

day4 :: Day
day4 = day (length . bruteForce isPassword) (length . bruteForce isPassword')
