{-# LANGUAGE Rank2Types #-}
module AOC
    ( day
    , Day(..)
    )
where

wrap :: (Read i, Show o) => (i -> o) -> (String -> String)
wrap f = show . f . read

day :: (Read i, Show o) => (i -> o) -> (i -> o) -> Day
day p1 p2 = Day (wrap p1) (wrap p2)

data Day = Day
    { part1 :: String -> String
    , part2 :: String -> String
    }
