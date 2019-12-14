module Day7
    ( day7
    )
where

import           AOC
import           Day5
import           Data.List



-- putN :: Int -> AmplifierState -> LoopState ()
-- putN n a = do
--     as <- get
--     put $ take (n - 1) as ++ a : drop n as

-- initSystem :: Intcode -> [Int] -> LoopState ()
-- initSystem c ps =
--     let results = runProgram spec c <$> ps :: [ProgramStatus]
--         amps    = flip fmap results $ \res -> AmplifierState res (output res)
--     in  put amps

-- -- runAmps :: OpSpec -> Int -> LoopState Int
-- -- runAmps spec init = do
-- --     amps <- get
-- --     let pairs = zip amps (tail amps)
-- --         `fmap` pairs $ \(amp1, amp2) -> case amp1 of
-- --             RunningState _ o -> case runProgram spec ic [phase amp2, ] of
-- --                 Paused res pc ic -- { output :: Int, programCounter :: Int, intcode :: Intcode }

-- --     let sc = scanl scanf (init, undefined) amps :: [(Int, AmplifierState)]
-- --     put $ fmap snd sc
-- --     return . fst . last $ sc


-- day7 =
--     let
--         p1 c =
--             maximum $ fmap (runSequence $ prog c) (genSequences [0 .. 4] 5) :: Int
--         p2 c = runSequence (prog c) [4, 3, 2, 1, 0]
--     in
--         day p1 p2
day7 = undefined
