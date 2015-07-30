import GRPSeed as Genome

--USAGE: Change GRPSeed in the import above.
--The imported file should have a consistent module name without any '-'
--also, the stat file should exist

--TODO for usage of Genome's State variable: Add another CLI parameter that reads state and writes it to agentStats

import System.IO.Strict(readFile)

import GRPFitness
import System.Random
import GRPCommon
import System.Environment
import Debug.Trace
import GRPStats
import GRPSafety
import Data.Maybe (fromJust)
import Debug.Trace
import System.Directory

--This whole file is a hack. Once System.Plugin does as I please, this will not be needed.

--CLI options:
-- ./headless -e GRPGenome0.hs GRPGenome1.hs
--  Make Genome 0 generate a modified version of itself named Genome 1

-- ./headless -f GRPGenome0.hs
--  Evaluate the fitness value of Genome 0

main :: IO()
main = do
  putStrLn "headless was called"
  args <- getArgs
  putStrLn $ show args
  let statFile = (args !! 1) ++ ".stat"
  if head args == "-e" then evolve statFile (args !! 1) (args !! 2) else fitnessEval stateFile

fitnessEval :: FilePath -> IO()
fitnessEval stateFile =
  putStrLn "fitness was called"
  file <- System.IO.Strict.readFile statFile
  let oldStats = read file :: AgentStats
  (newFit, newState) <- computeProblemFitness Genome.act (state oldStats)
  putStrLn ("newFit = " ++ show newFit)
  let newStats = AgentStats (source oldStats) (Compilation, newFit) (ancestry oldStats) (generation oldStats) newState False (evaluatedChildren oldStats) (compiledChildren oldStats) :: AgentStats
  writeFile (statFile ++ "~") $show newStats
  renameFile (statFile ++ "~") statFile
  --TODO2: verify correct copying


evolve :: FilePath -> FilePath -> FilePath -> IO()
evolve parentStatFile srcFile newFileName = do
  putStrLn "ev was called"
  putStrLn "stuff"
  pStatString <- System.IO.Strict.readFile parentStatFile
  let pStat = read pStatString :: AgentStats
  src <- System.IO.Strict.readFile srcFile
  rng <- newStdGen
  let (newCode, newState) = reprogram [rng] (state pStat) [fromJust $ dropSafetyPrefix src]
  if newCode == ( fromJust $ dropSafetyPrefix src ) -- primitive external duplicate Control, Mk. 2
  then do
    putStrLn "Welp! That's a duplicate!"
    undefined --TODO: This is possibly a bit hacky.
  else do
    writeFile newFileName ("--{-# LANGUAGE Safe #-}\nmodule " ++ (reverse $ drop 3 $ reverse newFileName) ++ "\n" ++ (unlines $ drop 2 $ lines $ fromJust $ getSafetyPrefix src) ++ newCode)
    let stats = AgentStats ("./" ++ newFileName) (Unchecked, 0.0) (createAncestry pStat) (1+ generation pStat) [] False 0 0 :: AgentStats
    writeFile (newFileName ++ ".stat") $ show stats
    putStrLn "ev terminated"
