module TypeCheckProblem(
  fitness
, generateInput
, worstScore
, bestScore
, generateTestCases
, readTestCases
, writeTestCases
) where

import GRPCommon
import System.Random
import Control.Monad
import Data.List
import GRPMath
import Dictionary
import GhciEval

--TODO: This file will generate Haskell expressions as Strings,
--give information about used functions, typeclasses etc and then
--expect the candidate to tell whether the given expression typechecks.
--it seems like a good idea to weight test cases so that non-type-checking
--tests have less of an impact.
--The function should be applicable to expression first and foremost,
--though it would be beneficial to also enable checking of whole functions
--the returned fitness value should establish a viable, learnable slope

--something along the lines of "echo \input\ | ghci"
--also, it will be useful to use "echo ":browse Prelude" | ghci"

--example I/O: In = ["map :: (a->b) -> [a] -> [b]", "(+ 1) :: Num a -> a -> a", "[1..3] :: (Enum t, Num t) => [t]]
--Out: map (+1)  [1..3] :: (Enum b, Num b) => [b]
--fitness: section in front of arrow is bonus, the closer the better. Low impact on fitness
--double-colon is required; highish impact.
--section after :: - every matching parameter adds points, every right constraint, correct result type; high total impact

--TODO: Generate a set of Inputs on/pre startup, load from File; to avoid huge runtime of hint.
--TODO: Look into starting up hint globally to prevent long startup time.

testset explength datalength = do
  exps <- forM [1..datalength] (\_ -> generateInput explength) :: IO [Input]
  let dataset = map (\(TCI [x]) -> x) exps
  doesTypeCheck <- fmap (map (/= "")) $ mapM eval dataset
  return (length $ filter id doesTypeCheck, length doesTypeCheck)

writeTestCases :: [(Input, Output)] -> IO ()
writeTestCases lst = writeFile "TypeCheckTests" $ show lst

readTestCases :: IO [(Input, Output)]
readTestCases = liftM read $ readFile "TypeCheckTests"

generateTestCases :: Int -> IO ()
generateTestCases n = do
  lstOfInputs <- mapM generateInput [div x 300 + 1 | x <- [1..n]]
  lstOfOutputs <- mapM (\(TCI inp) -> eval $ head inp) lstOfInputs
  --TODO: wrap this in a lookup container, make that available to fitness... somehow.
  --writeFile "TypeCheckTests" $ show (zip lstOfInputs map $ TCO lstOfOutputs)
  writeTestCases $ zip lstOfInputs (map TCO lstOfOutputs)
  putStrLn "Hello world 1"
  print lstOfInputs
  putStrLn "Hello world 2"
  print lstOfOutputs
  putStrLn "Hello world 3"
  --results <- zipWithM fitness lstOfInputs $ map TCO lstOfOutputs
  --print results
  putStrLn "Hello world 4"
  return ()

lookUpTestCase :: String -> IO Output
lookUpTestCase input =
  do
    assocs <- readTestCases
    return $ snd $ head $ filter (\(TCI x, _) -> head x == input) $ assocs

fitness :: Input -> Output -> IO Float
fitness (TCI inp) (TCO out) = do
  (TCO actualType) <- lookUpTestCase $ head inp
  print actualType
  return $ similarity actualType out
fitness (PPI _) (PPO _) = error "You called the wrong function, doofus."

--TODO: Satisfies minimally necessary conditions. Not very smooth, reacts harshly to smaller errors.
--TODO: Needs to ignore type variable naming.
similarity :: String -> String -> Float
similarity a b = fromIntegral correctWords / fromIntegral numWords
  where
    numWords = max (length $ words a) (length $ words b)
    correctWords = length $ filter id $ zipWith (==) (words a) (words b)

--TODO: Needs to intentionally generate more complex input structures: Brackets, indentation, type annotations, etc.
generateInput :: Int -> IO Input
generateInput 1 = do
  (word, _) <- getStdRandom (pickRandomly declarations)
  return $ TCI [word]
generateInput i = do
  (word, _) <- getStdRandom (pickRandomly declarations)
  (TCI [rest]) <- generateInput (i-1)
  return $ TCI [word ++ " " ++ rest]

worstScore :: Input -> Float
worstScore x = 0.0

bestScore :: Input -> Float
bestScore x = 1.0
