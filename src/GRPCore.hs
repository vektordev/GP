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

import Control.Applicative
import Control.Concurrent.ParallelIO.Global

import Debug.Trace

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

deep :: IO()
deep = do
  let params = Settings "./GRPSeed.hs" "./result" 500 30 300
  ag <- initializeSeed $ initialAgent params
  let iPool = (initialPool ag (poolMin params) (poolMax params)) :: Pool
  compiledPool <- evaluateFitness iPool
  iteratePool (generations params) compiledPool (resultpath params)

test :: IO()
test = do
  let params = Settings "./GRPSeed.hs" "./result" 1 5 30
  ag <- initializeSeed $ initialAgent params
  let iPool = (initialPool ag (poolMin params) (poolMax params)) :: Pool
  compiledPool <- evaluateFitness iPool
  iteratePool (generations params) compiledPool (resultpath params)

main :: IO()
main = do
  let params = Settings "./GRPSeed.hs" "./result" 100 30 300
  ag <- initializeSeed $ initialAgent params
  let iPool = (initialPool ag (poolMin params) (poolMax params)) :: Pool
  compiledPool <- evaluateFitness iPool
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
  stopGlobalPool --this is about the thread pool...
iteratePool n pool dump = do
  putStrLn "filtered at begin: "
  writeFile (dump ++ show n) $ show pool
  writeFile (dump ++ "hr" ++ show n) $ unlines $ map (\(AgentStats path _ ancestry _ _ _ evalCh compCh) -> "Agent: " ++ path ++ ", ratio " ++ (show compCh) ++  "%" ++ (show evalCh) ++ ", ancestry: " ++ show ancestry ) $ agents pool
  showPool pool;
  (crashedParents, fullP) <- refillPool pool
  putStrLn "Done refilling"
  evalP <- evaluateFitness fullP
  putStrLn "with Fitness values: "
  showPool evalP
  iteratePool (n-1) (filterPool evalP) dump

initialPool :: AgentStats -> Int -> Int -> Pool
initialPool ags min max = Pool max min 1 [ags] []

--transforms from human-readable format to gobbledygook
--removes intra-function newlines double newlines; then removes all the double spaces.
--transformCode :: String -> String
--transformCode readable = foldl () readable

initializeSeed :: FilePath -> IO AgentStats
initializeSeed path = do
  --Copy source file
  src <- System.IO.Strict.readFile path
  writeFile "./GRPGenome0.hs" ("{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ (unlines $ drop 2 $ lines src))
  --Use generator to generate a valid headless file for the source file
  generate "./GRPGenome0.hs"
  --create a stats file for the base genome.
  let ag = AgentStats "./GRPGenome0.hs" (Unchecked, 0.0) [] 1 [] True 0 0 -- Using True, because this one doesn't have a parent, so no need to.
  writeFile "./GRPGenome0.hs.stat" $ show ag
  return ag

showPool (Pool _ _ _ ags _) = do
  putStrLn "\n\n\n"
  _ <- sequence $ map (\(AgentStats path fitness _ gen _ _ evalCh compCh)
    -> putStrLn ( path ++ "; fitness: " ++ (show fitness) ++ " from generation " ++ (show gen) ++ "; ratio: " ++ (show (compCh, evalCh))) ) ags
  putStrLn "\n\n\n"
  return ()

refillPool :: Pool -> IO ([FilePath], Pool)
refillPool (Pool max f id agents oldags) = do
  let parents = take (max - length agents) $ concat $ repeat $ reverse $ sort agents
  putStrLn ("creating " ++ (show (max - length agents)) ++ " children")
  --this could possibly be parallelized:
  --However, if one Genome produces several offspring, they should be sequenced.
  --In other words, we can generate each genome's children sequentially, and can do that in parallel for all the parent genomes..
  children <- sequence (map (\(p, id) -> createChild p p id) $ zip parents [id..])--TODO2: This is kinda risky, as I am not entirely confident I won't end up with duplicate IDs.
  putStrLn ("Children: " ++ ( show (lefts children)))
  return $ (rights children, Pool max f (id + length (lefts children)) ((lefts children) ++ agents) oldags)
--Very basic approach: each leftover parent generates children, starting with the best, until all slots are filled.
--More sophisticated methods with certain biases for genetic diversity and better fitness values need to be tested.

createChild :: AgentStats -> AgentStats-> Int -> IO (Either AgentStats FilePath)
--Assumption: Has already been compiled.
--TODO1:
createChild codeAg@(AgentStats path fit ancestry generation state _ _ _) (AgentStats src _ _ _ _ _ _ _) id = do
  let destname = "GRPGenome" ++ show id ++ ".hs"
  let executable = (reverse $ drop 3 $ reverse path) ++ "hl"
  putStrLn ("trying to generate id:" ++ (show id))
  --call Evolve - write resulting source code and stats file.
  --(code, out, err) <- readProcessWithExitCode ((reverse $ drop 3 $ reverse path) ++ "hl") ["-e", src, destname] "" --deprecated
  writeFile (path ++ ".stat") $show codeAg
  (code, out, err) <- readProcessWithExitCode "timeout" ["10s", executable, "-e", src, destname] "" --with timeout
  if (code == ExitSuccess)
  then do
    generate destname --generate Headless file
    dump <- System.IO.Strict.readFile (destname ++ ".stat")
    putStrLn ("dump read for agent: " ++ show id)
    return $ Left $ read dump
  else do
    --TODO: This part should definitely increment the agent's failure count! See also digestFeedback.
    --The way it is currently, if the agent fails to execute evCall, this won't be counted as failure.
    --Thus, the trashiest of compiling agents remain in the pool. BAD!
    printEv ("errors generated by evCall on " ++ path ++ ":")
    printEv $ show code
    printEv out
    printEv err
    return $ Right path

printEv :: String -> IO()
printEv str = putStrLn str
--printEv str = return ()

--sequence operation for every agent: cast fitness on it - if returned agent is at least compilation-fit. This will compile the executable.
--Then call the executable to eval the problem-specific fitness, assuming compilation was achieved.
--Map the off-the-shelf fitness function. This will compile the genome. Then, call the headless executable to generate the problem-specific fitness value.
--TODO:
--    This function probably needs to imprintEv str = plement functionality to cast back the fitness of a genome to it's parent. Thus, parents producing non-compiling offspring are less likely to reproduce.
--    This regulation needs to respect the overall probability of generating a good genome, so both the quota of compilation and the fitness values of offspring need to be considered.
--    There needs to be a clearly defined way in which a genome's fitness influences it's ancestry that doesn't involve infinite loopback.
evaluateFitness :: Pool -> IO Pool
evaluateFitness (Pool max min id agents oldags) =
  do
    evalResult <- parallel $ map (evalGenome) agents
    let (ags, feedback) = unzip evalResult
    let newAgs = digestFeedback ags $ concat feedback
    putStrLn ("newPool" ++ ( show newAgs ))
    return (Pool max min id newAgs oldags)

digestFeedback :: [AgentStats] -> [(Bool, FilePath)] -> [AgentStats]
--TODO: This is a n^2 function, could be n log n by applying sorting.
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
--TODO: extract feedback data into pool: Assign feedback to agents.
--foldl (\agentlist feedbackItem -> map (\agent -> if source agent == snd feedback then  else agent) agentlist) agents feedback

--As per the above TODO mark, how about AgentStats -> IO (AgentStats, MoreData)
--Where MoreData must carry: Is the Genome compilable? If so, did it terminate?
--If so, how good does it perform? Basically, if this genome has never been evaluated before, a fitness value is needed and will be used when dealing with the parent.
--TODO: Needs to return quickly if the genome has been eval'd before.
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
      putStrLn ("ok " ++ path)
      let executable = (reverse $ drop 3 $ reverse path) ++ "hl"
      --(code, out, err) <- readProcessWithExitCode executable ["-f", path] "" --blocking execution
      (code, out, err) <- readProcessWithExitCode "timeout" ["10s", executable, "-f", path] "" --now with timeout
      if code /= ExitSuccess then putStrLn ("A genome failed to execute fitness properly\n" ++ err) else return ()
      newDump <- System.IO.Strict.readFile statfile
      return (read newDump, if not fitToParent then [(True, parent ag)] else []) --return compiled agent; good feedback to the parent
    else do
      printFit ("This genome failed to compile " ++ path)
      return (AgentStats path fitNew ancestry generation state fitToParent eval comp, if not fitToParent then [(False, parent ag)] else []) --bad feedback to the parent.

printFit :: String -> IO()
printFit str = return ()
--printFit str = putStrLn str

--TODO: removeDuplicates :: Pool -> IO Pool

--Junk - no compilation
--unfit: more than 600 failed offspring
--unfit: sorted out by pressure of other agents
--fit: none of the above

filterPool :: Pool -> Pool
filterPool (Pool m fSize id ags oldags) = Pool m fSize id fit (unfit ++ oldags)
  where
    noJunk = filter (\ag -> (fst $ getFitness ag) >= Compilation) ags
    (f1, unfit1) = filter2 (\ag -> evaluatedChildren ag <1000 || compiledChildren ag > 0) noJunk
    (fit, unfit2) = if length f1 > fSize then (take fSize f1, drop fSize f1) else (f1, [])
    unfit = unfit1 ++ unfit2
    filter2 pred lst = ( filter pred lst, filter (not.pred) lst )
--  (filter
--    (\ag -> --select those that are at least compiling and that haven't had more than 500 failures without a success.
--      fst ( getFitness ag) >= Compilation
--    && (evaluatedChildren ag < 600 || compiledChildren ag > 0))
--    (take fSize (sortBy ( flip compare) ags))) --take the fSize best ags
