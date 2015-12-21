module GRPFitness
( Fitness
, Level(Unchecked, Unsafe, RuntimeErrOnParent, UnknownCompilerError, Compilation)
, computeProblemFitness
, computeFitness
) where

import System.Exit (ExitCode(..))

import Data.List

import Control.Monad

import GRPSafety
import Debug.Trace
import System.Process (readProcessWithExitCode, createProcess, shell)
import System.Random
import GRPCommon
import PartitioningProblem
import GRPMath

{-
  This file handles all things fitness, and by extension, safety.
  computeProblemFitness is used to create a fitness value for a given act function. This is generally used by GRPHeadless.
  computeFitness is called by GRPCore and ensures safety properties of the source code and then compiles the genome.
-}

data Level =
  Unchecked |
  Unsafe |
  RuntimeErrOnParent |
  MiscErr |
  ParseErr |
  ScopeErr |
  AmbiguousSymbolErr |
  TypeErr |
  NoBindingError |
  UnknownCompilerError |
  Compilation deriving (Show, Read, Eq, Ord)
--first one is the "fitness level" that was achieved. Second one is the score within that level.
--For most of the levels, this will be const 0, until fine grained detail in those areas is required.
type Fitness = (Level, Float)
--this representation might be a bit harder to visualize overall than a single continuous value.
--However, it is much easier than to fit a error count without an upper bound into a fixed interval.

fitOut :: String -> IO()
fitOut str = return ()

--trace str x = x --temporarily mute the trace calls

-- source code -> assigned future filepath ->
-- Fitness Value and a list of errors to point out to the parent (and their magnitude)
computeFitness :: String -> FilePath -> IO (Fitness, [(Int, String)])
computeFitness source path = do
  fitOut ("trying to compile path " ++ path)
  let (safeness, safetymsg) = isSafe source
  if not safeness
  then do
    fitOut ("Genome Unsafe" ++ safetymsg)
    return ((Unsafe, 0.0), [(-10, safetymsg)]) -- fitness: safety violation
  else do
    --TODO: err and out are completely disregarded.
    (code, out, err) <- readProcessWithExitCode "ghc" [path] []
    fitOut "Compiler output: "
    fitOut out
    fitOut err
    if code == ExitSuccess
    then do
      fitOut"compilation achieved"
      return ((Compilation, -1.0 * (2^126)), []) -- fitness: compilation achieved, but no further details known
      --TODO: Compute base fitness here: Code length, Hlint, runtime of mutate, performance of mutate (call and use shallow eval)
      --This function will not implement performance in a specific problem domain.
    else do
      fitOut ("no compilation " ++ show (length err))
      fitOut err
      return $ compileErrorFitness out err -- fitness and feedback on failed compilation

--TODO 2: query multiple times with different input data; get median/mean.
--This can be extended to evaluate fitness in different problem domains.
computeProblemFitness :: ([StdGen] -> State -> Input -> (Output, State)) -> State -> IO (Float, State)
computeProblemFitness actFnc agState = do
  let queries = [2..11]
  rngs <- replicateM (length queries) newStdGen
  inputs <- mapM generateInput [2^n | n <- queries]
  let (newSt, outs)= foldr (\ (input, rng) (state, oldouts) -> let (actOut, actState) = actFnc [rng] state input in (actState, actOut : oldouts)) (agState, []) (zip inputs rngs) :: (State, [Output])  --actFnc [rng] agState input
  fits <- zipWithM fitness inputs outs
  return (mean $ zipWith normalizeFitness inputs fits, newSt) --(sum (map normalizeFitness inputs fit) / (length queries), newSt)

normalizeFitness :: Input -> Float -> Float
normalizeFitness input rawFitValue =
  (rawFitValue - worstScore input) / (bestScore input - worstScore input)

--This needs to aggregate the errors and process them.
compileErrorFitness :: String -> String -> (Fitness, [(Int, String)])
compileErrorFitness out err
  | "Conflicting definitions for" `isInfixOf` err = ((ScopeErr, -(fromIntegral $ length err)), [])
  | "Illegal tuple section" `isInfixOf` err = ((MiscErr, -(fromIntegral $ length err)), [])
  | "No instance for" `isInfixOf` err = ((TypeErr, -(fromIntegral $ length err)), [])
  | "lacks an accompanying binding" `isInfixOf` err = ((NoBindingError, -(fromIntegral $ length err)),[])
  | "must have lower precedence than that of the operand" `isInfixOf` err = ((MiscErr, -(fromIntegral $ length err)),[])
  | "Ambiguous occurrence" `isInfixOf` err = ((AmbiguousSymbolErr, -(fromIntegral $ length err)),[])
  | "Couldn't match" `isInfixOf` err = ((TypeErr, -(fromIntegral $ length err)),[])
  | "arguments, but has been given" `isInfixOf` err = ((TypeErr, -(fromIntegral $ length err)),[])
  | "is applied to too many type arguments" `isInfixOf` err = ((MiscErr, -(fromIntegral $ length err)),[])
  | "cannot construct the infinite type:" `isInfixOf` err = ((MiscErr, -(fromIntegral $ length err)),[])
  | "parse error" `isInfixOf` err = ((ParseErr, -(fromIntegral $ length err)),[])
  | "Parse error" `isInfixOf` err = ((ParseErr, -(fromIntegral $ length err)),[])
  | "Not in scope" `isInfixOf` err = ((ScopeErr, -(fromIntegral $ length err)),[])
  | "have different numbers of arguments" `isInfixOf` err = ((TypeErr, -(fromIntegral $ length err)),[])
  | "Non type-variable argument in the constraint:" `isInfixOf` err = ((TypeErr, -(fromIntegral $ length err)),[])
  | otherwise = trace ("GRPFitness: Unknown compiler error: " ++ err) ((UnknownCompilerError, -(fromIntegral $ length err)),[])
