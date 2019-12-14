module Intcode.Types where

import           Control.Monad.State
import           Text.Printf
import           Parser

newtype Intcode = Intcode { getIntcode :: [Int] } deriving Show
parseIntcode :: ReadP Intcode
parseIntcode = Intcode <$> sepBy1 integer (char ',')

instance Read Intcode where
    readsPrec _ = readP_to_S parseIntcode

type Dest = Int
type Val = Int

type Mode = Int
data Result = Set Dest Val
            | Input Dest
            | Continue
            | Output Int
            | Jump Int
            | End
            deriving (Show, Eq)
data Opcode = Opcode { opcode :: Int, modes :: [Int] } deriving Show

data Op = WOp  { arity :: Int, runWOp :: Int -> Result }
        | ROp  { arity :: Int, runROp :: [Int] -> Result }
        | RWOp { arity :: Int, runRWOp :: [Int] -> Int -> Result }

instance Show Op where
    show (WOp a _) = printf "WOp { arity = %i, runWOp :: Int -> Result }" a
    show (ROp a _) = printf "ROp { arity = %i, runROp :: [Int] -> Result }" a
    show (RWOp a _) =
        printf "RWOp { arity = %i, runRWOp :: [Int] -> Int -> Result}" a


newtype OpSpec = OpSpec { getOp :: Int -> Op }

instance Show OpSpec where
    show _ = "OpSpec"

data Param = Immediate Int | Position Int deriving Show

data Instruction = RInstruction { op :: Op, args :: [Param] }
                 | WInstruction { op :: Op, dest :: Int }
                 | RWInstruction { op :: Op, args :: [Param], dest :: Int }

data ProgramStatus = Waiting | Ended | Error deriving (Eq,Show)
data ProgramData =  ProgramData
    { _spec :: OpSpec
    , _programCounter :: Int
    , _intcode :: Intcode
    , _status :: ProgramStatus
    , _output :: Maybe Int
    , _input :: Maybe Int
    } deriving Show

type ProgramState = State ProgramData
