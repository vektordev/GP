import GRPSeed as Genome

--USAGE: Change GRPSeed in the import above.
--The imported file should have a consistent module name without any '-'
--also, the stat file should exist

--TODO for usage of Genome's state: Add another CLI parameter that reads state and writes it to agentStats

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

--CLI option to call evolve
--main: generate AgentStats by performing X on Y
-- ./headless -e GRPGenome0.hs GRPGenome1.hs
-- ./headless -f GRPGenome0.hs
main :: IO()
main = do
  putStrLn "headless was called"
  args <- getArgs
  putStrLn $ show args
  let statFile = (args !! 1) ++ ".stat"
  if head args == "-e" then evolve statFile (args !! 1) (args !! 2) else do
    putStrLn "fitness was called"
    file <- readFile statFile
    let oldStats = read file :: AgentStats
    (newFit, newState) <- computeProblemFitness Genome.act (state oldStats)
    putStrLn ("newFit = " ++ show newFit)
    let newStats = AgentStats (source oldStats) (Compilation, newFit) (ancestry oldStats) (generation oldStats) newState False :: AgentStats
    writeFile (statFile ++ "~") $show newStats --Always false, since we're recomputing, we might as well.
    renameFile (statFile ++ "~") statFile
    --TODO2: verify correct copying

evolve :: FilePath -> FilePath -> FilePath -> IO()
evolve parentStatFile srcFile newFileName = do
  putStrLn "ev was called"
  putStrLn "stuff"
  pStatString <- readFile parentStatFile
  let pStat = read pStatString :: AgentStats
  src <- readFile srcFile --drop safetyprefix
  rng <- newStdGen
  let (newCode, newState) = reprogram [rng] (state pStat) [fromJust $ dropSafetyPrefix src] 
  writeFile newFileName ("{-# LANGUAGE Safe #-}\nmodule " ++ (reverse $ drop 3 $ reverse newFileName) ++ "\n" ++ (unlines $ drop 2 $ lines $ fromJust $ getSafetyPrefix src) ++ newCode)
  let stats = AgentStats ("./" ++ newFileName) (Unchecked, 0.0) (createAncestry pStat) (1+ generation pStat) [] False :: AgentStats
  writeFile (newFileName ++ ".stat") $ show stats
  putStrLn "ev terminated"
