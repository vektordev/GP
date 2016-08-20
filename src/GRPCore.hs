--CRITICAL: Genomes are currently not compiled as safe. Relevant changes include GRPSeed, GRPSeedhr, Headless line 59 and Core line 134.
--This is so Language.Haskell.Exts.Parser is viable to use.

import GRPSeed
import GRPCommon
import GRPFitness
import GRPSafety
import PartitioningProblem
import GRPStats
import GRPGenerator

import System.Environment (getArgs)
import System.Random
import System.Directory
import System.Process (readProcessWithExitCode, createProcess, shell)
import System.Exit (ExitCode(..))
import System.IO.Strict(readFile)

import Data.Maybe
import Data.List
import Data.Either
import Data.Function
import Data.Tree
import Data.Ord

import Control.Applicative

import Debug.Trace

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

{-
  This file is the core of the application.
  The usual procedure here is to pick GRPSeed.hs, make GRPGenome0.hs from it's source,
    generate AgentStats for it and save it to GRPGenome0.hs.stat, and generate GRPGenome0hl.hs based on GRPHeadless
    That single genome shall be our 0th generation.
  From here on, the application keeps refilling the genome pool to it's full size and reducing it to filtered size, both given as part of settings.
  Various debug informatiion will be printed and the application will save a copy
    of the Pool (does not contain genomes directly, but their file paths) as well as a dotfile displaying parent-child relations.
-}

--TODO: Implement abstract interfaces for:
-- Continuous compound simulations
-- Discrete compound simulations
-- Discrete single-agent tasks
--Compound simulations being fitness measures where there's direct competition - for example chess in versus mode(discrete), or a ALife simulations (continuous)
--single-agent tasks are static fitness benchmarks for agents

data Pool = Pool {
  maxSize :: Int,
  filteredSize :: Int,
  nextID :: Int,
  agents :: [AgentStats],
  oldAgents :: [AgentStats]
} deriving (Show, Read)

data Settings = Settings {
  initialAgent :: FilePath,
  resultpath :: FilePath,
  generations :: Int,
  poolMin :: Int,
  poolMax :: Int
}

--This can be used to improve the compile rate of children of the root genome. Just testing around.
haskellSrcExtsTest :: IO()
haskellSrcExtsTest = do
  source <- System.IO.Strict.readFile "./GRPSeed.hs"
  let parseResult = parseModule source
  let ast = fromParseResult parseResult
  putStrLn $show ast
  putStrLn "\n\n\n"
  putStrLn $ prettyPrint ast

main :: IO()
main = do
  let params = Settings "./GRPSeed.hs" "./result" 50 30 600
  ag <- initializeSeed $ initialAgent params
  let iPool = (initialPool ag (poolMin params) (poolMax params)) :: Pool
  compiledPool <- evaluateFitness iPool []
  iteratePool (generations params) compiledPool (resultpath params)

toDotFile :: Pool -> String
toDotFile (Pool _ _ _ ags oldags) = unlines ( "strict graph network {" : (map arrow (ags ++ oldags) ) ++ (map label (ags ++ oldags)) ++ ["}"] )
  where
    arrow ag = if not $ null $ ancestry ag then "\t" ++ (shortName $ source ag) ++ "--" ++ (shortName $ head $ ancestry ag) ++ ";" else ""
    shortName name = drop 2 $ reverse $ drop 3 $ reverse name
    label ag =
      "\t" ++ (shortName $ source ag)
      ++ "[label=\"" ++ ( shortName $ source ag )
      ++ " " ++ ( show $ compiledChildren ag )
      ++ " / " ++ ( show $ evaluatedChildren ag ) ++ "\"];"

iteratePool :: Int -> Pool -> FilePath -> IO()
iteratePool 0 pool dump = do
  writeFile (dump ++ "hr") $ unlines $ map (\(AgentStats path _ ancestry _ _ _ evalCh compCh) -> "Agent: " ++ path ++ ", ratio " ++ (show compCh) ++  "%" ++ (show evalCh) ++ ", ancestry: " ++ show ancestry ) $ agents pool
  writeFile dump $ show pool
  writeFile "ancestry.dot" $ toDotFile pool
  showPool pool
  stopGlobalPool --stop the thread pool - not the gene pool.
iteratePool n pool dump = do
  putStrLn "filtered at begin: "
  writeFile (dump ++ show n) $ show pool
  writeFile (dump ++ show n ++ ".dot") $ toDotFile pool
  writeFile (dump ++ "hr" ++ show n) $ unlines $ map (\(AgentStats path _ ancestry _ _ _ evalCh compCh) -> "Agent: " ++ path ++ ", ratio " ++ (show compCh) ++  "%" ++ (show evalCh) ++ ", ancestry: " ++ show ancestry ) $ agents pool
  showPool pool;
  (crashedParents, fullP) <- refillPool pool
  evalP <- evaluateFitness fullP crashedParents
  iteratePool (n-1) (filterPool evalP) dump

initialPool :: AgentStats -> Int -> Int -> Pool
initialPool ags min max = Pool max min 1 [ags] []

initializeSeed :: FilePath -> IO AgentStats
initializeSeed path = do
  --Copy source file
  src <- System.IO.Strict.readFile path
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ (unlines $ drop 2 $ lines src))
  --Use generator to generate a valid headless file for the source file
  generate "./GRPGenome0.hs"
  --create a stats file for the base genome.
  let ag = AgentStats "./GRPGenome0.hs" (Unchecked, 0.0) [] 1 [] True 0 0 -- Using True, because this one doesn't have a parent, so no need to.
  writeFile "./GRPGenome0.hs.stat" $ show ag
  return ag

showPool (Pool _ _ _ ags _) = do
  putStrLn "\n\n\n"
  _ <- mapM (\(AgentStats path fitness _ gen _ _ evalCh compCh)
    -> putStrLn ( path ++ "; fitness: " ++ (show fitness) ++ " from generation " ++ (show gen) ++ "; ratio: " ++ (show (compCh, evalCh))) ) ags
  putStrLn "\n\n\n"
  return ()

--TODO: Implement a duplicate-check for genome names in Pool.

--Following are two options of generating new genomes.

--combines every agent's code gen with every agent's source code.
--This can exceed the pool's max capacity, filling up to min capacity squared.
--As a remedy to that problem (a already full pool handed to cartesianProduct might just require too much computation time..), a token system should be used.
cartesianProduct :: Pool -> IO ([FilePath], Pool)
cartesianProduct (Pool max f id agents oldags) = do
  let coderSourcePairs = [(coder, source) | coder <- agents, source <- agents]
  children <- mapM (\(genomeID, (c, s)) -> createChild c s genomeID) $ zip [id..] coderSourcePairs
  return $ (rights children, Pool max f (id + length coderSourcePairs) ((lefts children) ++ agents) oldags)

--Possible remedy: Use the algorithm in GRPMath
refillPool :: Pool -> IO ([FilePath], Pool)
refillPool (Pool max f id agents oldags) = do
  --TODO: We can generate each genome's children sequentially, and can do that in parallel for all the parent genomes..
  children <- mapM (\(p, id) -> createChild p p id) $ zip parents [id..]
  return $ (rights children, Pool max f (id + length parents) ((lefts children) ++ agents) oldags)
  where
    tokens = max - length agents
    parentFit = sortBy (comparing snd ) $ map (\ag -> (ag, getAggregateFitness ag) ) agents
    sumOfFits = sum $ map snd parentFit
    tokenPerFit = (fromIntegral tokens) / sumOfFits
    agTokens = map (\ (ag, fit) -> (ag, round( tokenPerFit * fit ) ) ) parentFit
    parents = concatMap (\ (ag, tok) -> replicate tok ag) agTokens

--Very basic approach: each leftover parent generates children, starting with the best, until all slots are filled.
--More sophisticated methods with certain biases for genetic diversity and better fitness values need to be tested.

--Takes a genome as codeGen and a genome as base source and generates offspring
--If generating a offspring fails, the parent genome will be punished, indicated by returning "Right *it's name*"
--punishment will be handled when the aggregate result is injected into evaluateFitness
--Assumption: Codegen has already been compiled.
createChild :: AgentStats -> AgentStats-> Int -> IO (Either AgentStats FilePath)
createChild codeAg@(AgentStats path fit ancestry generation state _ _ _) (AgentStats src _ _ _ _ _ _ _) id = do
  let destname = "GRPGenome" ++ show id ++ ".hs"
  let executable = (reverse $ drop 3 $ reverse path) ++ "hl"
  --If the above is exchanged for a static executable, one can have a static code generator to optimize the genomes.
  --call Evolve - generate resulting source code and stats file.
  writeFile (path ++ ".stat") $show codeAg
  (code, out, err) <- readProcessWithExitCode "timeout" ["1s", executable, "-e", src, destname] "" --with timeout
  if (code == ExitSuccess)
  then do
    generate destname --generate Headless file
    dump <- System.IO.Strict.readFile (destname ++ ".stat")
    return $ Left $ read dump
  else
    if (code == ExitFailure 124) then do --error code of timeout
      putStrLn ("the genome " ++ (show path) ++ " hit timeout when generating!")
      --Maybe, this is the place to heuristically detect infinilooping programs.
      return $ Right path --I would really like to punish this way more!
    else do
      printEv ("errors generated by evCall on " ++ path ++ ":")
      printEv $ show code
      printEv out
      printEv err
      return $ Right path

printEv :: String -> IO()
printEv str = return() --Just a shortcut to mute a lot of printLns.

--Map operation over agents: cast fitness on it - if returned agent is at least compilation-fit. This will compile the executable.
--Done in parallel.
--Then call the executable to eval the problem-specific fitness, assuming compilation was achieved.
--The evalGenome function returns data on whether a child
--This also keeps feedback on each Genome about the compilation rate of it's children.
--TODO:
--    The mechanism by which producing non-compiling offspring will result in punishing of the parent needs to respect fitness values of the children generated too.
evaluateFitness :: Pool -> [FilePath] -> IO Pool
evaluateFitness (Pool max min id agents oldags) crashedParents =
  do
    evalResult <- parallel $ map (evalGenome) agents
    let (ags, feedback) = unzip evalResult
    let newAgs = digestFeedback ags ((concat feedback) ++ (zip (repeat False) crashedParents))
    printEv ("newPool" ++ ( show newAgs ))
    return (Pool max min id newAgs oldags)

--This function increments the counters for evaluated children and compiled children according to whether the newly evaluated child did compile.
--TODO: This is a n^2 function, could be n log n by applying sorting.
digestFeedback :: [AgentStats] -> [(Bool, FilePath)] -> [AgentStats]
digestFeedback ags fb =
  let
    applyOneFeedbackItem agentlist feedbackItem =map (maybeApplyItemToAgent feedbackItem) agentlist
    maybeApplyItemToAgent item agent =
      if source agent == snd item
      then agent {
        evaluatedChildren = 1 + evaluatedChildren agent,
        compiledChildren = (if fst item then 1 else 0) + compiledChildren agent
      }
      else agent
  in foldl applyOneFeedbackItem ags fb

--This function evaluates the fitness value of a Genome.
--It also returns the parent's name if the genome didn't compile.
--This is tracked and accounted for when assigning tickets for new offspring.
--  Maybe we want the fitness value noise of evaluating a genome every iteration?
evalGenome :: AgentStats -> IO (AgentStats, [(Bool, String)])
evalGenome ag@(AgentStats path fitOld ancestry generation state fitToParent eval comp) =
  if (fst fitOld) > Unchecked
  then do
    printFit "nothing to evaluate here"
    return (ag, [])
  else do
    printFit ("evaluating a genome " ++ path)
    let statfile = path ++ ".stat"
    src <- System.IO.Strict.readFile path
    (fitNew, errors) <- computeFitness src ((reverse $ drop 3 $ reverse path) ++ "hl.hs")
    writeFile statfile $show $AgentStats path fitNew ancestry generation state fitToParent eval comp
    if fitNew >= (Compilation, -1.0 * (2^127))
    then do--process call to headless: Evaluate!
      printFit ("ok " ++ path)
      let executable = (reverse $ drop 3 $ reverse path) ++ "hl"
      (code, out, err) <- readProcessWithExitCode "timeout" ["1s", executable, "-f", path] "" --now with timeout
      if code /= ExitSuccess then putStrLn ("A genome failed to execute fitness properly\n" ++ err) else return ()
      newDump <- System.IO.Strict.readFile statfile --read the AgentStats as modified by the child process.
      return (read newDump, if not fitToParent then [(True, parent ag)] else []) --return compiled agent; good feedback to the parent
    else do
      printFit ("This genome failed to compile " ++ path)
      return (AgentStats path fitNew ancestry generation state fitToParent eval comp, if not fitToParent then [(False, parent ag)] else []) --bad feedback to the parent.

printFit :: String -> IO()
printFit str = return () --same as above, mutes all fitness-related printlines.


--filters the pool according to the following criteria:
--Junk - no compilation -> remove entirely
--unfit: more than allowedStrikes failed offspring
--unfit: sorted out by being unfit relative to other agents
--  ->Move these two categories to the oldAgents section.
--Keep them for monitoring purposes.
--fit: none of the above
filterPool :: Pool -> Pool
filterPool (Pool m fSize id ags oldags) = Pool m fSize id fit (unfit ++ oldags)
  where
    allowedStrikes = 100
    noJunk = filter (\ag -> (fst $ getFitness ag) >= Compilation) ags
    (f1, unfit1) = filter2 (\ag -> evaluatedChildren ag < allowedStrikes || compiledChildren ag > 0) noJunk
    (fit, unfit2) = if length f1 > fSize then (take fSize f1, drop fSize f1) else (f1, [])
    unfit = unfit1 ++ unfit2
    filter2 pred lst = ( filter pred lst, filter (not.pred) lst )
