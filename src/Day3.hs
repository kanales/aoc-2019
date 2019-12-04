module Day3 where

import           AOC
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , readP_to_S
                                                , sepBy1
                                                , char
                                                )
import           Parser
import           Data.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad.State

data Direction = L | R | U | D deriving (Show)
data Instruction = Instruction { dir :: Direction, steps :: Int } deriving (Show)

parseInstruction :: ReadP Instruction
parseInstruction = Instruction <$> fmap match uppercase <*> integer
  where
    match 'L' = L
    match 'R' = R
    match 'U' = U
    match 'D' = D

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

evaluate :: [Instruction] -> [Instruction] -> Int
evaluate is1 is2 = flip evalState Map.empty $ do
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

delays :: [Instruction] -> [Instruction] -> Int
delays is1 is2 = flip evalState Map.empty $ do
    insertWire is1
    ins <- intersections is2
    dis <- forM ins $ \(x, d1) -> do
        d2 <- gets (Map.lookup x)
        return $ maybe d1 (+ d1) d2
    return $ minimum dis

p1 :: String -> Int
p1 s = let (is1 : is2 : _) = parseInstructions <$> lines s in evaluate is1 is2

p2 :: String -> Int
p2 s = let (is1 : is2 : _) = parseInstructions <$> lines s in delays is1 is2

day = Day p1 p2
