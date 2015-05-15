module GRPTests
( test
) where

import GRPSeed
import GRPFitness
import System.Random
import GRPCommon
import PartitioningProblem

--TODO: THis needs a lot of work, sometime.

test :: IO ()
test = do
  fitnessStability act [] 30 30

fitnessStability :: ([StdGen] -> State -> Input -> (Output, State)) -> State -> Int -> Int -> IO ()
fitnessStability act state testCount testLength= do
  inputs <- sequence $ take testCount $ repeat (generateInput testLength) :: IO [Input]
  rng <- newStdGen
  let outputs = map fst $ map (act [rng] state) inputs
  let fitValues = map (\(i,o) -> fitness i o) (zip inputs outputs)
  putStrLn $ show fitValues
