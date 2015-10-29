module GRPPool (

) where

--import GRPStats
import GRPFitness
import GRPGenerator
import GRPMath
import GRPIndividual

import System.IO.Strict(readFile)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (removeFile)

import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Ord
import Data.List

--currently supports only one single root node. This can be changed later on.
--no State currently within Individual.
--
--this needs to support:
--  toDotFile - trivial
--  iteratePool - agnostic of internal structure
--  initialPool - trivial
--  showPool - map?
--  cartesianProduct - rather easy, needs insertion of node
--  refillPool - bit complicated, needs insertion of node
--    insertion of node
--  evaluateFitness - plain old map
--
--in order to be able to truncate the top of the ancestry tree safely while
--maintaining nice properties (same behavior as without truncating),
--we need to ensure that the truncated parts cannot become better than the
--currently active part of the pool.

data Pool = Pool {
  maxSize :: Int,
  filteredSize :: Int,
  nextID :: Int, -- since we're a bit generous with these, we need to count genomes otherwise.
  --so some general statistics would be in order.
  genomes :: Tree Individual
} deriving (Show, Read)

filterPool :: Pool -> Pool
filterPool (Pool max min nID population) =
  if length (flatten population) > min
  then Pool max min nID updatedPopulation
  else Pool max min nID updatedPopulation'
  where
    updatedPopulation' = fmap removeJunk population
    threshold          = getFitness $ (!!) (sortBy (flip compare) (flatten population)) min
    updatedPopulation  = fmap (updateIndividual threshold) updatedPopulation'

main = do
  putStrLn "Starting test run."
  ip <- initialPool
  newPool <- iteratePool ip 5
  putStrLn "Done!"

initialPool :: IO Pool
initialPool = do
  src <- System.IO.Strict.readFile "./GRPSeed.hs"
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ unlines ( drop 2 $ lines src))
  generate "./GRPGenome0.hs"
  --This function does NOT write .stat to disk. This is done before calls to the executable.
  return (Pool 100 10 1 (Node (ActiveI (Unchecked, 0.0) "./GRPGenome0") []))

iteratePool :: Pool -> Int -> IO Pool
iteratePool p 0 = return p
iteratePool p it = do
  putStrLn "Starting iteration: "
  print p
  ep <- evaluateFitness p
  print ep
  let fp = filterPool ep
  print fp
  rp <- refillPool fp
  print rp
  iteratePool rp (it-1)

--cartesianProduct :: Pool -> IO Pool

refillPool :: Pool -> IO Pool
refillPool (Pool max min nxt genomes) = do
  let size = length $ flatten genomes
  let newGenomesCnt = max - size
  --We definitely need to preprocess the fitness value
  -- - bare as they are, they're not really good for that purpose
  --negative weights are particularly bad.
  let tickets = weightedAssign newGenomesCnt (fmap (abs . snd . getFitness) genomes)
  return $ Pool max min nxt genomes

createAllChildren :: Tree Individual -> Tree [(Int, Maybe FilePath)] -> IO (Tree Individual)
--general idea: To zipper, iterate. Recurse on leftmost child, add own children if applicable.
--recurse on right neighbor. If no right neighbor, return
createAllChildren iTree tTree = do
  upd <- zipperCreateAll (fromTree iTree)(fromTree tTree)
  return $ toTree upd

zipperCreateAll :: TreePos Full Individual -> TreePos Full [(Int, Maybe FilePath)] -> IO(TreePos Full Individual)
zipperCreateAll indZipper ticketZipper = return indZipper

createChild :: TreePos Full Individual -> Int -> FilePath -> IO(TreePos Full Individual)
createChild loc id srcCode = do
  let
    mkChild ind@(ActiveI fit path) id srcCode = do
      writeFile (path ++ ".hs.stat") $show ind
      (code, out, err) <- readProcessWithExitCode "timeout"
        ["1s", path ++ "hl", "-e", srcCode ++ ".hs", "./GRPGenome" ++ show id ++ ".hs"] ""
      System.Directory.removeFile (path ++ ".hs.stat") --state would be lost here
      if code == ExitSuccess
        then do
          generate ("GRPGenome" ++ show id)
          return [Node (ActiveI (Unchecked, 0.0) ("GRPGenome" ++ show id)) []]
        else return []
  newElem <- mkChild (rootLabel $ tree loc) id srcCode
  return (modifyTree (\(Node a subnodes) -> Node a (newElem ++ subnodes)) loc)

--this function is going to become tricky later on. Right now, it's just a plain old
--map over the tree structure, but later we'll do a traversion of the neighborhood
--for every node considered.
--also, Fitness is likely to not cut it anymore later on as a type.
evaluateFitness :: Pool -> IO Pool
evaluateFitness pool = do
  genomes' <- sequence $ fmap evalIndividual (genomes pool)
  return (pool{genomes = genomes'})

--a function :: (TreePos Individual Full -> Individual) ->
--  (TreePos Individual Full) -> (TreePos Individual Full)
--will come in handy
--  first argument gets a function to extract the relevant information from the neighborhood.
--  essentially maps from one Tree to the other
--this looks like a Functor instance for TreePos Full essentially.

evalIndividual :: Individual -> IO Individual
evalIndividual (ActiveI (Unchecked, val) path) = do
  src <- System.IO.Strict.readFile (path ++ ".hs")
  (fitVal, hints) <- computeFitness src (path ++ "hl.hs")
  if fitVal >= (Compilation, -1.0 * (2^127))
  then do
    writeFile (path ++ ".hs.stat") (show (ActiveI (Unchecked, val) path))
    (code, out, err) <- readProcessWithExitCode "timeout" ["1s", path ++ "hl", "-f", path ++ ".hs"] ""
    if code == ExitSuccess
    then do
      newFitStr <- System.IO.Strict.readFile (path ++ ".hs.stat")
      let (ActiveI newFit path) = read newFitStr
      System.Directory.removeFile (path ++".hs.stat")
      return (ActiveI newFit path)
    else do
      putStrLn "exit failure"
      print (code, out, err)
      --System.Directory.removeFile (path ++".hs.stat")
      return (ActiveI (Compilation, -1.0 * (2^64)) path)
  else return (ActiveI fitVal path)
--Marking individuals as inactive shouldn't be done here.
evalIndividual indiv = return indiv
--do nothing if already evaluated or inactive/junk (which implies already evaluated)

--a = putStrLn $ drawTree $ fmap show $ toTree $ modifyTree (\tree2789 -> Node 111 []) $ fromJust $ childAt 1 $ fromTree test
--a = drawTree $ fmap show $ toTree $ delete $ fromJust $ childAt 1 $ fromTree test
