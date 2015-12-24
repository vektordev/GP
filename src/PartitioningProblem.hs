module PartitioningProblem
( fitness
, generateInput
, worstScore
, bestScore
) where

import Data.List
import System.Random

import GRPCommon

--fitness function for the partitioning problem, as well as a random generator for input.

fitness :: Input -> Output -> IO Float
fitness (PPI input) (PPO out) =
  if sort input == sort (fst out ++ snd out) --is it a valid partitioning?
  then return $ - (fromIntegral $ abs ((sum $ fst out) - (sum $ snd out)))
  else return $ - (fromIntegral $ sum $ map abs input)
fitness _ _ = error "wrong input type"

generateInput :: Int -> IO Input
generateInput 0 = return $ PPI []
generateInput n = do
  x <- getStdRandom next
  (PPI xs) <- generateInput (n-1)
  return $ PPI (x:xs)

worstScore :: Input -> Float
worstScore (PPI []) = 0
worstScore (PPI (i:inp)) = - ((fromIntegral $ abs $ length (i:inp)) * (fromIntegral $ snd $ genRange $ mkStdGen 0))
worstScore _ = error "wrong input type"

bestScore :: Input -> Float
bestScore (PPI _) = 0
bestScore _ = error "wrong input type"
