module Day3
    ( day3
    , Input
    )
where

import           AOC
import           Parser                  hiding ( get )
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.State

data Direction = L | R | U | D deriving (Show)
data Instruction = Instruction { dir :: Direction, steps :: Int } deriving (Show)

parseInstruction :: ReadP Instruction
parseInstruction =
    let match 'L' = L
        match 'R' = R
        match 'U' = U
        match 'D' = D
    in  Instruction <$> fmap match uppercase <*> integer

data Input = Input { getInput :: ([Instruction],[Instruction]) } deriving (Show)

parseInput :: ReadP Input
parseInput =
    curry Input
        <$> commaSep parseInstruction
        <*  char '\n'
        <*> commaSep parseInstruction

instance Read Input where
    readsPrec _ = readP_to_S parseInput

parseInstructions :: String -> [Instruction]
parseInstructions = fst . last . readP_to_S p
    where p = sepBy1 parseInstruction (char ',')

step :: (Int, Int) -> Instruction -> ([(Int, Int)], (Int, Int))
step (x, y) (Instruction dir dis) = case dir of
    L -> ([ (x - i, y) | i <- [1 .. dis] ], (x - dis, y))
    U -> ([ (x, y + i) | i <- [1 .. dis] ], (x, y + dis))
    R -> ([ (x + i, y) | i <- [1 .. dis] ], (x + dis, y))
    D -> ([ (x, y - i) | i <- [1 .. dis] ], (x, y - dis))

toTuples :: [Instruction] -> [(Int, Int)]
toTuples = concatMap fst . tail . scanl (\(_, p) i -> step p i) ([], (0, 0))

type BoardState = Map (Int, Int) Int

insertTuples :: [(Int, Int)] -> State BoardState ()
insertTuples ts = do
    s <- get
    put . fst $ foldl foldFun (s, 0) ts
    where foldFun (k, d) v = (Map.insert v d k, d + 1)

insertWire :: [Instruction] -> State BoardState ()
insertWire = insertTuples . toTuples

tryIntersection :: (Int, Int) -> State BoardState Bool
tryIntersection (x, y) = gets $ Map.member (x, y)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

evaluate :: Input -> Int
evaluate (Input (is1, is2)) = flip evalState Map.empty $ do
    insertWire is1
    ins <- intersections is2
    return . minimum . fmap (manhattan . fst) $ ins

-- Part 2
intersections :: [Instruction] -> State BoardState [((Int, Int), Int)]
intersections is = do
    let tups = toTuples is
    fst <$> foldM foldFun ([], 1) tups
  where
    foldFun (ins, d) x = do
        b <- tryIntersection x
        if b then return ((x, d + 1) : ins, d + 1) else return (ins, d + 1)

delays :: Input -> Int
delays (Input (is1, is2)) = flip evalState Map.empty $ do
    insertWire is1
    ins <- intersections is2
    dis <- forM ins $ \(x, d1) -> do
        d2 <- gets (Map.lookup x)
        return $ maybe d1 (+ d1) d2
    return $ minimum dis

day3 = day evaluate delays
