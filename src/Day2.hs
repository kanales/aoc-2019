module Day2
    ( day2
    )
where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Maybe
import           AOC
import           Parser

newtype Intcode = Intcode { getIntcode :: [Int] }

parseIntcode :: ReadP Intcode
parseIntcode = Intcode <$> sepBy1 integer (char ',')

instance Read Intcode where
    readsPrec _ = readP_to_S parseIntcode

transform :: ([Int] -> [Int]) -> Intcode -> Intcode
transform f = Intcode . f . getIntcode

setInputs :: Int -> Int -> Intcode -> Intcode
setInputs noun verb = transform $ \c -> head c : noun : verb : drop 3 c

procIntcode :: V.Vector Int -> V.Vector Int
procIntcode init = runST $ do
    let len = V.length init
    v <- do
        v <- V.thaw init
        let
            recf v i = when (i < len) $ do
                op <- VM.read v i
                when (op == 1 || op == 2) $ do
                    x <- VM.read v (i + 1) >>= VM.read v
                    y <- VM.read v (i + 2) >>= VM.read v
                    d <- VM.read v (i + 3)
                    if op == 1
                        then VM.write v d (x + y)
                        else VM.write v d (x * y)
                    recf v (i + 4)
        recf v 0
        return v
    V.freeze v

p1 :: Intcode -> Int
p1 c = procIntcode (V.fromList $ getIntcode c) V.! 0

bruteForcePairs :: Int -> [(Int, Int)]
bruteForcePairs n = (,) <$> [0 .. n] <*> [0 .. n]

p2 :: Intcode -> Int
p2 c = 100 * noun + verb
  where
    ff (n, v) = p1 (setInputs n v c) == 19690720
    (noun, verb) = fromJust $ find ff (bruteForcePairs 99)

day2 = day (p1 . setInputs 12 2) p2
