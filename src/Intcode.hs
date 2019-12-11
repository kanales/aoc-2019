module Intcode
    ( Dest
    , Val
    , Mode
    , Status(..)
    , Opcode
    , Op(..)
    , OpSpec(..)
    , setInputs
    , evalProgram
    , Intcode(..)
    )
where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Parser
import           Debug.Trace

trace' s x = trace (s ++ " " ++ show x) x

newtype Intcode = Intcode { getIntcode :: [Int] } deriving Show
parseIntcode :: ReadP Intcode
parseIntcode = Intcode <$> sepBy1 integer (char ',')

instance Read Intcode where
    readsPrec _ = readP_to_S parseIntcode


transform :: ([Int] -> [Int]) -> Intcode -> Intcode
transform f = Intcode . f . getIntcode


setInputs :: [Int] -> Intcode -> Intcode
setInputs inputs = transform $ \c -> head c : inputs ++ drop 3 c

type Dest = Int
type Val = Int

type Mode = Int
data Status = Set Dest Val
            | Input
            | Continue
            | Output Int
            | Jump Int
            | End
            deriving (Show, Eq)

data Opcode = Opcode { opcode :: Int, modes :: [Int] } deriving Show

processOpcode :: Int -> Opcode
processOpcode inp =
    let ds = show inp
        pad i = if length i < 5 then pad ('0' : i) else i
        [m3, m2, m1, o1, o2] = pad ds
        o = read [o1, o2] :: Int
        modes = (read . return <$> [m1, m2, m3]) ++ [0 ..] :: [Int]
    in  Opcode o modes

data Param = Immediate Int | Position Int deriving Show

getParam :: (Mode, Int) -> Param
getParam (0, i) = Position i
getParam (1, i) = Immediate i

evalParam :: VM.MVector s Int -> Param -> ST s Int
evalParam v (Position  i) = VM.read v i
evalParam v (Immediate i) = return i

data Instruction = RInstruction { op :: Op, args :: [Param] }
                 | WInstruction { op :: Op, dest :: Int }
                 | RWInstruction { op :: Op, args :: [Param], dest :: Int }

data Op = WOp  { arity :: Int, runWOp :: Int -> Status }
        | ROp  { arity :: Int, runROp :: [Int] -> Status }
        | RWOp { arity :: Int, runRWOp :: [Int] -> Int -> Status }
newtype OpSpec = OpSpec { getOp :: Int -> Op }

mkInstruction :: Op -> [Mode] -> [Int] -> Instruction
mkInstruction op ms vals = case op of
    WOp a _ -> WInstruction op (head vals)
    ROp a _ -> RInstruction op $ getParam <$> zip ms vals
    RWOp a _ ->
        let pars = getParam <$> zip ms (init vals) :: [Param]
        in  RWInstruction op pars (last vals)

execInstruction :: VM.MVector s Int -> Instruction -> ST s Status
execInstruction v ins = case ins of
    RInstruction (ROp _ r) args -> r <$> mapM (evalParam v) args
    WInstruction (WOp _ r) arg  -> return $ r arg
    RWInstruction (RWOp _ r) args dest ->
        r <$> mapM (evalParam v) args <*> return dest

procIntcode :: OpSpec -> V.Vector Int -> Int
procIntcode spec c =
    let init = c
    in  runST $ do
            v <- V.thaw init
            o <- recf spec v 0 Nothing
            v <- V.freeze v
            case o of
                Just x  -> return x
                Nothing -> (V.!) <$> return v <*> return 0

slice :: VM.MVector s a -> Int -> Int -> ST s [a]
slice v offset n = VM.read v `mapM` [offset .. offset + n - 1]

recf :: OpSpec -> VM.MVector s Int -> Int -> Maybe Int -> ST s (Maybe Int)
recf spec v pc o = do
    Opcode oc ms <- processOpcode <$> VM.read v pc
    let op = getOp spec oc
    let n  = arity op
    vals <- slice v (pc + 1) n
    let ins = mkInstruction op ms vals
    res <- execInstruction v ins
    case res of
        Set d x -> do
            VM.write v d x
            recf spec v (pc + n + 1) o
        Output o'  -> recf spec v (pc + n + 1) (Just o')
        Jump   pos -> recf spec v pos o
        Continue   -> recf spec v (pc + n + 1) o
        End        -> return o

evalProgram :: OpSpec -> Intcode -> Int
evalProgram spec c = procIntcode spec (V.fromList $ getIntcode c)
