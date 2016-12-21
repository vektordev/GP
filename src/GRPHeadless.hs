import GRPSeed as Genome

--USAGE: Change GRPSeed in the import above.
--The imported file should have a consistent module name without any '-'
--also, the stat file should exist

--TODO for usage of Genome's State variable: Add another CLI parameter that reads state and writes it to agentStats

import System.IO.Strict(readFile)

import GRPIndividual
import GRPFitness
import GRPCommon
import GRPStats
import GRPSafety

import Data.Maybe (fromJust)

import Debug.Trace

import System.Random
import System.Directory
import System.Environment

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
  if head args == "-e" then evolve statFile (args !! 2) (args !! 3) else fitnessEval statFile

fitnessEval :: FilePath -> IO()
fitnessEval statFile = do
  putStrLn "fitness was called"
  file <- System.IO.Strict.readFile statFile
  let oldStats = read file :: Individual
  (newFit, newState) <- computeProblemFitness Genome.act []
  putStrLn ("newFit = " ++ show newFit)
  let newStats = setFitness oldStats (Compilation, newFit)
  writeFile (statFile ++ "~") $show newStats
  renameFile (statFile ++ "~") statFile
  --TODO2: verify correct copying

-- -e
evolve :: FilePath -> FilePath -> FilePath -> IO()
evolve parentStatFile srcFile newFileName = do
  putStrLn "ev was called"
  putStrLn "stuff"
  pStatString <- System.IO.Strict.readFile parentStatFile
  let pStat = read pStatString :: Individual
  src <- System.IO.Strict.readFile srcFile
  rng <- newStdGen
  let (newCode, newState) = reprogram [rng] [] [fromJust $ dropSafetyPrefix src]
  if newCode == fromJust (dropSafetyPrefix src ) -- primitive external duplicate Control, Mk. 2
  then
    error "Welp! That's a duplicate!"
  else do
    --newState will, at some point, have to be written back.
    writeFile newFileName ("--{-# LANGUAGE Safe #-}\nmodule " ++ (reverse $ drop 3 $ reverse newFileName) ++ "\n" ++ (unlines $ drop 2 $ lines $ fromJust $ getSafetyPrefix src) ++ newCode)
    --The following .stat file won't offer new info. Just return to confirm termination.
    --let stats = ActiveI (Unchecked, 0.0) ("./" ++ newFileName) -- (createAncestry pStat) (1+ generation pStat) [] False 0 0 :: AgentStats
    --writeFile (newFileName ++ ".stat") $ show stats
    putStrLn "ev terminated"
