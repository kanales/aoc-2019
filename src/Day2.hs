module Day2 (day) where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           AOC
import           Parser
import           Text.ParserCombinators.ReadP

type Intcode = [Int]

parseIntcode :: String -> Intcode
parseIntcode = fst . last . readP_to_S p
    where p = sepBy1 integer (char ',') :: ReadP Intcode

setInputs :: Int -> Int -> Intcode -> Intcode
setInputs noun verb c = head c : noun : verb : (drop 3 c)

procIntcode :: V.Vector Int -> V.Vector Int
procIntcode init = runST $ do
        let len = V.length init
        v <- do
            v <- V.thaw init
            -- recf :: VM.MVector s Int -> Int -> ST s ()
            let recf v i = if (i < len)
                then do
                    op <- VM.read v i
                    if (op == 1 || op == 2)
                        then do
                            x <- VM.read v (i + 1) >>= VM.read v
                            y <- VM.read v (i + 2) >>= VM.read v
                            d <- VM.read v (i + 3)
                            if (op == 1)
                                then VM.write v d (x + y)
                                else VM.write v d (x * y)
                            recf v (i + 4)
                        else return ()
                else return ()
            recf v 0
            return v
        V.freeze v
    
p1' :: Intcode -> Intcode
p1' c = V.toList $ procIntcode (V.fromList c)

p1 :: Intcode -> Int
p1 c = procIntcode (V.fromList c) V.! 0

bruteForcePairs :: Int -> [(Int, Int)]
bruteForcePairs n = (,) <$> [0..n] <*> [0..n]

p2 :: Intcode -> Int
p2 c = 100 * noun + verb
    where   ff (n,v)     = (p1 $ setInputs n v c) == 19690720
            (noun, verb) = fromJust $ find ff (bruteForcePairs 99)

day = Day 
    { part1 = p1 . (setInputs 12 2) . parseIntcode
    , part2 = p2 . parseIntcode 
    }
