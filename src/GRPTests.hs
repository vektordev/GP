module GRPTests
( test
) where

import GRPSeed
import GRPFitness
import System.Random
import GRPCommon
import PartitioningProblem
import GRPMath

--There should really be some unit-tests or so here.
--Currently, all this file does is monitor the means and variances of the fitness values of various act functions.
--This is so the parameters for the fitness checks of actual genomes can be validated to be reasonably accurate.

test :: IO ()
test = do
  let (cnt, lng) = (500,20)
  (m1,v1) <- fitnessStability act [] cnt lng
  (m3,v3) <- fitnessStability goodNAct [] cnt lng
  (m2,v2) <- fitnessStability badButBetter [] cnt lng
  putStrLn ("means: " ++ (show (m1/m2)) ++ " variance: " ++ (show(v1/v2)))

fitnessStability :: ([StdGen] -> State -> Input -> (Output, State)) -> State -> Int -> Int -> IO (Float, Float)
fitnessStability act state testCount testLength= do
  inputs <- sequence $ take testCount $ repeat (generateInput testLength) :: IO [Input]
  rng <- newStdGen
  let outputs = map fst $ map (act [rng] state) inputs
  fitValues <- mapM (\(i,o) -> fitness i o) (zip inputs outputs)
  --putStrLn $ show fitValues
  putStrLn ("mean: " ++ show (mean fitValues) ++ " stdDev: " ++ show (sqrt $ variance fitValues) ++ " relative Dev: " ++ (show  ((sqrt $ variance fitValues) / (mean fitValues))))
  return (mean fitValues, variance fitValues)

goodNAct :: [StdGen] -> State -> Input -> (Output, State)
goodNAct rngs state inp = (foldr (\num (a, b) -> if sum a < sum b then ( (num:a) , b) else (a, (num:b) ) ) ([],[]) inp , state)

badButBetter :: [StdGen] -> State -> Input -> (Output, State)
badButBetter rngs state (a:b:xs) =
  let (x,y) = ( take ( div ( length xs ) 2 ) xs, drop ( div ( length xs ) 2 ) xs )
  in
    if (sum x > sum y)
    then
      if sum x > sum (a:y)
      then ((x    , a:b:y), state)
      else ((b:x  , a:y  ), state)
    else
      if sum (a:x) > sum y
      then ((a:x  , b:y  ), state)
      else ((a:b:x, y    ), state)
