module Day3 where

import           AOC
import           Text.ParserCombinators.ReadP   ( ReadP
                                                , readP_to_S
                                                , sepBy1
                                                , char
                                                )
import           Parser
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Control.Monad.State

data Direction = L | R | U | D deriving (Show)
data Instruction = Instruction { dir :: Direction, steps :: Int } deriving (Show)

parseInstruction :: ReadP Instruction
parseInstruction = do
    d <- match <$> uppercase
    i <- integer
    return $ Instruction d i
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

insertTuples :: [(Int, Int)] -> State (Set (Int, Int)) ()
insertTuples ts = do
    s <- get
    put $ foldl (flip Set.insert) s ts

insertWire :: [Instruction] -> State (Set (Int, Int)) ()
insertWire = insertTuples . toTuples


tryIntersection :: (Int, Int) -> State (Set (Int, Int)) Bool
tryIntersection (x, y) = Set.member (x, y) <$> get

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

evaluate :: [Instruction] -> [Instruction] -> Int
evaluate is1 is2 = flip evalState Set.empty $ do
    insertWire is1
    let tups = toTuples is2
    ins <- filterM tryIntersection tups
    return . minimum . fmap manhattan $ ins


p1 :: String -> Int
p1 s = let (is1 : is2 : _) = parseInstructions <$> lines s in evaluate is1 is2

day = Day p1 undefined
