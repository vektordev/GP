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
import Data.Traversable

import Control.Monad

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
  if length (filter (\i -> case i of JunkI _ -> False; _ -> True) $ flatten population) > min
  then Pool max min nID updatedPopulation
  else Pool max min nID updatedPopulation'
  where
    updatedPopulation' = fmap removeJunk population
    threshold          = getFitness $ (!!) (sortBy (flip compare) (flatten population)) (min - 1)
    updatedPopulation  = fmap (updateIndividual threshold) updatedPopulation'

main = do
  putStrLn "Starting test run."
  ip <- initialPool
  newPool <- iteratePool ip 20
  putStrLn "Done!"

poolSummary :: Pool -> IO ()
poolSummary p = do
  let genP = flatten $ genomes p
  putStrLn "pool Summary:"
  putStrLn $ drawTree $ fmap show $ genomes p
  --print $ length $ genomes p
  putStrLn "-+-+-+-+-+-+-+-+-+-+-+-+-+-"
  print $ filter (\i -> case i of
    ActiveI (Compilation, f) p -> True;
    InactiveI (Compilation, f) p -> True;
    _ -> False) genP  --(\i -> case i of JunkI _ -> False; InactiveI _ _ -> False; ActiveI Compilatio p -> ) genP
  putStrLn "---\n"

initialPool :: IO Pool
initialPool = do
  src <- System.IO.Strict.readFile "./GRPSeed.hs"
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ unlines ( drop 2 $ lines src))
  generate "./GRPGenome0.hs"
  --This function does NOT write .stat to disk. This is done before calls to the executable.
  return (Pool 10 1 1 (Node (ActiveI (Unchecked, 0.0) "./GRPGenome0") []))

iteratePool :: Pool -> Int -> IO Pool
iteratePool p 0 = return p
iteratePool p it = do
  putStrLn "Starting iteration: "
  poolSummary p
  ep <- evaluateFitness p
  poolSummary ep
  let fp = filterPool ep
  poolSummary fp
  rp <- refillPool fp
  poolSummary rp
  iteratePool rp (it-1)

--cartesianProduct :: Pool -> IO Pool

refillPool :: Pool -> IO Pool
refillPool (Pool max min nxt genomes) = do
  let size = length $ filter (\i -> case i of ActiveI _ _ -> True; _ -> False) $ flatten genomes
  let newGenomesCnt = max - size
  let tickets = [nxt .. nxt + newGenomesCnt - 1]
  --We definitely need to preprocess the fitness value
  -- - bare as they are, they're not really good for that purpose
  --negative weights are particularly bad. Thus: Absolute value, just to be sure.
  let wtNodes = weightedAssign newGenomesCnt (fmap (\i -> case i of ActiveI (Compilation,f) _ -> abs f; _ -> 0) genomes)
  let ticketnodes = fmap (\x -> zip x (repeat Nothing) ) $ assignTickets wtNodes tickets
  newGenomes <- createAllChildren genomes ticketnodes
  return $ Pool max min (nxt + newGenomesCnt) newGenomes

--assignTickets :: Tree Int -> [Int] -> Tree [Int]
assignTickets tree tickets =
  let (x, ticketNodes) = mapAccumR (\tickets weight -> if length tickets < weight then error "Too few tickets in assignTickets" else (drop weight tickets, take weight tickets)) tickets tree
  in if null x then ticketNodes else error "Too many tickets in assignTickets"

createAllChildren :: Tree Individual -> Tree [(Int, Maybe FilePath)] -> IO (Tree Individual)
--general idea: To zipper, iterate. Recurse on leftmost child, add own children if applicable.
--recurse on right neighbor. If no right neighbor, return
createAllChildren iTree tTree = do
  upd <- zipperCreateAll (fromTree iTree)(fromTree tTree)
  return $ toTree upd

--needs to iterate over all children off the two treePos.
--Then, call createChild as often as required by root ticket node.
--recursion scheme: Call leftmost child.
--Return val ensures that all children and transitive children have been dealt with.
--Then, recurse on right brother if possible.
--Call createChild on self with required tickets.
--Important: Self-call in last place to ensure no new children have been created to call on.
zipperCreateAll :: TreePos Full Individual -> TreePos Full [(Int, Maybe FilePath)] -> IO(TreePos Full Individual)
zipperCreateAll indZipper ticketZipper = do
  lowerRec <-
    if hasChildren indZipper
    then liftM
      (fromJust . parent)
      (zipperCreateAll
        (fromJust $ firstChild indZipper)
        (fromJust $ firstChild ticketZipper))
    else return indZipper
  rightRec <-
    if not $ isLast lowerRec
    then liftM (fromJust . prev) $ zipperCreateAll (fromJust $ next lowerRec) (fromJust $ next ticketZipper)
    else return lowerRec
  zipperCreateLocal rightRec (label ticketZipper)

zipperCreateLocal :: TreePos Full Individual -> [(Int, Maybe FilePath)] -> IO (TreePos Full Individual)
zipperCreateLocal t [] = return t
zipperCreateLocal tree ((i, Nothing):ts) =
  case path $ label tree of
    Nothing -> zipperCreateLocal tree ts
    Just x ->createChild tree i x >>= flip zipperCreateLocal ts
zipperCreateLocal tree ((i, Just path):ts) = createChild tree i path >>= flip zipperCreateLocal ts

--handles insertion into tree and failure of -e process.
createChild :: TreePos Full Individual -> Int -> FilePath -> IO(TreePos Full Individual)
createChild loc id srcCode = do
  let
    mkChild ind@(ActiveI fit path) id srcCode = do
      writeFile (path ++ ".hs.stat") $show ind
      (code, out, err) <- readProcessWithExitCode "timeout"
        ["1s", path ++ "hl", "-e", srcCode ++ ".hs", "GRPGenome" ++ show id ++ ".hs"] ""
      System.Directory.removeFile (path ++ ".hs.stat") --state would be lost here
      if code == ExitSuccess
        then do
          generate ("GRPGenome" ++ show id ++ ".hs")
          return [Node (ActiveI (Unchecked, 0.0) ("./GRPGenome" ++ show id)) []]
        else return []
  newElem <- mkChild (rootLabel $ tree loc) id srcCode -- rootlabel . tree == label ?
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
      putStrLn "exit failure in eval"
      print (code, out, err)
      --System.Directory.removeFile (path ++".hs.stat")
      return (ActiveI (Compilation, -1.0 * (2^120)) path)
  else return (ActiveI fitVal path)
--Marking individuals as inactive shouldn't be done here.
evalIndividual indiv = return indiv
--do nothing if already evaluated or inactive/junk (which implies already evaluated)

--a = putStrLn $ drawTree $ fmap show $ toTree $ modifyTree (\tree2789 -> Node 111 []) $ fromJust $ childAt 1 $ fromTree test
--a = drawTree $ fmap show $ toTree $ delete $ fromJust $ childAt 1 $ fromTree test
