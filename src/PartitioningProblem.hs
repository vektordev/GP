module PartitioningProblem 
( fitness
, generateInput
) where

import Data.List
import System.Random

--TODO 3: replace type with data. Possibly, we'll need data constructors later on...??
type Input = [Int]
type Output = ([Int], [Int])

--TODO 2:
fitness :: Input -> Output -> Float
fitness input out = if sort input == sort (fst out ++ snd out)
  then - (fromIntegral $ abs ((sum $ fst out) - (sum $ snd out)))
  else - (fromIntegral $ sum $ map abs input)

generateInput :: Int -> IO Input
generateInput 0 = return []
generateInput n = do
  x <- getStdRandom next
  xs <- generateInput (n-1)
  return (x:xs)
