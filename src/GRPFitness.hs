module GRPFitness
( Fitness
, Level(Unchecked, Unsafe, UnkownCompilerError, Compilation)
, computeProblemFitness
, computeFitness
) where

import System.Exit (ExitCode(..))

import GRPSafety
--import Debug.Trace
import System.Process (readProcessWithExitCode, createProcess, shell)
import System.Random
import GRPCommon
import PartitioningProblem

--first one is the "fitness level" that was achieved. Second one is the score within that level.
data Level = Unchecked | Unsafe | UnkownCompilerError | Compilation deriving (Show, Read, Eq, Ord) --TODO: deriving show, read, Comparable?
type Fitness = (Level, Float)
--this representation might be a bit harder to visualize overall than a single continuous value. However, it is much easier than to fit a error count without an upper bound into a fixed interval.

fitOut :: String -> IO()
fitOut str = return ()--putStrLn str

trace str x = x --temporarily mute the trace calls

-- source code -> assigned future filepath ->
-- Fitness Value and a list of errors to point out to the parent (and their magnitude)
-- TODO: I don't really like the trace() here
computeFitness :: String -> FilePath -> IO (Fitness, [(Int, String)])
computeFitness source path = do
  let (safeness, safetymsg) = trace ("trying to compile path " ++ path) isSafe source
  if not safeness
  then return $ trace ("Genome Unsafe" ++ safetymsg)((Unsafe, 0.0), [(-10, safetymsg)]) -- fitness: safety violation
  else do
    (code, out, err) <- readProcessWithExitCode "ghc" [path] []
    fitOut "Compiler output: "
    fitOut out
    fitOut err
    if code == ExitSuccess
    then return $ trace "compilation achieved" ((Compilation, -1.0 * (2^126)), []) -- fitness: compilation achieved, but no further details known
      --TODO: Compute base fitness here: Code length, Hlint, runtime of mutate, performance of mutate (call and use shallow eval)
      --This function will not implement performance in a specific problem domain.
    else do
      fitOut ("no compilation " ++ (show $ length err))
      fitOut err
      return $ compileErrorFitness out err -- fitness and feedback on failed compilation

--TODO 2: query multiple times with different input data; get median/mean.
computeProblemFitness :: ([StdGen] -> State -> Input -> (Output, State)) -> State -> IO (Float, State)
computeProblemFitness actFnc agState = do
  rng <- newStdGen
  input <- generateInput 20
--TODO: Measure time the agent took to compute the result
  let (out, newSt) = actFnc [rng] agState input
  return (fitness input out, newSt)

compileErrorFitness :: String -> String -> (Fitness, [(Int, String)])
compileErrorFitness out err = trace ("Unknown compiler error: " ++ err) ((UnkownCompilerError,0.0),[])
