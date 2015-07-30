module PartitioningProblem
( fitness
, generateInput
) where

import Data.List
import System.Random

--fitness function for the partitioning problem, as well as a random generator for input.

type Input = [Int]
type Output = ([Int], [Int])

fitness :: Input -> Output -> Float
fitness input out =
  if sort input == sort (fst out ++ snd out) --is it a valid partitioning?
  then - (fromIntegral $ abs ((sum $ fst out) - (sum $ snd out)))
  else - (fromIntegral $ sum $ map abs input)

generateInput :: Int -> IO Input
generateInput 0 = return []
generateInput n = do
  x <- getStdRandom next
  xs <- generateInput (n-1)
  return (x:xs)
