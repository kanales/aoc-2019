module Intcode.Impl
    ( procIntcode
    )
where

import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import           Control.Monad.ST
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Control.Monad.State           as S

import           Intcode.Types

processOpcode :: Int -> Opcode
processOpcode inp =
    let ds = show inp
        pad i = if length i < 5 then pad ('0' : i) else i
        [m3, m2, m1, o1, o2] = pad ds
        o = read [o1, o2] :: Int
        modes = (read . return <$> [m1, m2, m3]) ++ [0 ..] :: [Int]
    in  Opcode o modes

getParam :: (Mode, Int) -> Param
getParam (0, i) = Position i
getParam (1, i) = Immediate i

evalParam :: VM.MVector s Int -> Param -> ST s Int
evalParam v (Position  i) = VM.read v i
evalParam v (Immediate i) = return i

mkInstruction :: Op -> [Mode] -> [Int] -> Instruction
mkInstruction op ms vals = case op of
    WOp a _ -> WInstruction op (head vals)
    ROp a _ -> RInstruction op $ getParam <$> zip ms vals
    RWOp a _ ->
        let pars = getParam <$> zip ms (init vals) :: [Param]
        in  RWInstruction op pars (last vals)

execInstruction :: VM.MVector s Int -> Instruction -> ST s Result
execInstruction v ins = case ins of
    RInstruction (ROp _ r) args -> r <$> mapM (evalParam v) args
    WInstruction (WOp _ r) arg  -> return $ r arg
    RWInstruction (RWOp _ r) args dest ->
        r <$> mapM (evalParam v) args <*> return dest

slice :: VM.MVector s a -> Int -> Int -> ST s [a]
slice v offset n = VM.read v `mapM` [offset .. offset + n - 1]

procIntcode :: ProgramData -> ProgramData
procIntcode state = runST $ do
    v      <- V.thaw . V.fromList $ getIntcode (_intcode state)
    state' <- recf state v
    v      <- V.freeze v
    return $ state' { _intcode = Intcode (V.toList v) }

recf :: ProgramData -> VM.MVector s Int -> ST s ProgramData
recf state v = do
    let pc = _programCounter state
    Opcode oc ms <- processOpcode <$> VM.read v pc
    let op = getOp (_spec state) oc
    let n  = arity op
    vals <- slice v (pc + 1) n
    let ins = mkInstruction op ms vals
    res <- execInstruction v ins
    case res of
        Set d x -> do
            VM.write v d x
            let state' = state { _programCounter = pc + n + 1 }
            recf state' v
        Jump pc' -> do
            let state' = state { _programCounter = pc' }
            recf state' v
        Continue -> do
            let state' = state { _programCounter = pc + n + 1 }
            recf state' v
        Output o -> do
            let state' =
                    state { _programCounter = pc + n + 1, _output = Just o }
            recf state' v
        Input d -> case _input state of
            Just i -> do
                VM.write v d i
                let state' = state { _programCounter = pc + n + 1 }
                recf state' v
            Nothing -> return $ state { _status = Waiting }
        End -> return $ state { _status = Ended }
