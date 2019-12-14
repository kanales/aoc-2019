{-# LANGUAGE LambdaCase #-}
module Intcode
    ( ProgramState
    , Result(..)
    , Intcode(..)
    , runProgram
    , execProgram
    , evalProgram
    , initialize
    , resume
    , hasEnded
    , get
    , put
    , Op(..)
    , OpSpec(..)
    )
where

import           Control.Monad.State     hiding ( put
                                                , get
                                                )
import qualified Control.Monad.State           as S
import           Intcode.Types
import           Intcode.Impl

get :: ProgramState ProgramData
get = S.get

put :: ProgramData -> ProgramState ()
put = S.put

initial :: OpSpec -> Intcode -> ProgramData
initial spec code = ProgramData { _spec           = spec
                                , _programCounter = 0
                                , _intcode        = code
                                , _status         = Waiting
                                , _output         = Nothing
                                , _input          = Nothing
                                }

runProgram
    :: OpSpec -> Intcode -> ProgramState (Maybe Int) -> (Maybe Int, Intcode)
runProgram spec code s =
    let state = s `execState` (initial spec code)
    in  (_output state, _intcode state)

evalProgram :: OpSpec -> Intcode -> ProgramState (Maybe Int) -> Maybe Int
evalProgram spec c = fst . runProgram spec c

execProgram :: OpSpec -> Intcode -> ProgramState (Maybe Int) -> Intcode
execProgram spec c = snd . runProgram spec c

execute :: ProgramState (Maybe Int)
execute = do
    state <- gets procIntcode
    S.put state
    return (_output state)

setInput :: Maybe Int -> ProgramState ()
setInput i = modify' $ \s -> s { _input = i }

initialize :: ProgramState (Maybe Int)
initialize = setInput Nothing >> execute

resume :: Int -> ProgramState (Maybe Int)
resume inp = setInput (Just inp) >> execute

hasEnded :: State ProgramData Bool
hasEnded = (`fmap` gets _status) $ \case
    Ended -> True
    _     -> False
