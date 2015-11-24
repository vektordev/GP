--import GRPStats
import GRPFitness
import GRPGenerator
import GRPMath
import GRPIndividual

import System.IO.Strict(readFile)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (removeFile)
import System.Environment (getArgs)

import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Ord
import Data.List
import Data.Traversable
import Data.Ratio

import Control.Monad
import Control.Concurrent.ParallelIO.Global

--currently supports only one single root node. This can be changed later on.
--no State currently within Individual.
--
--this needs to support:
-- +  toDotFile - trivial
--  iteratePool - agnostic of internal structure
--  initialPool - trivial
-- +  showPool - map?
-- +  cartesianProduct - rather easy, needs insertion of node
--  refillPool - bit complicated, needs insertion of node
--    insertion of node
--  evaluateFitness - plain old map
--
--  Inactive Individuals can become active again if a ActiveI's effective value
--    (after accounting for ancestral context) decreases over time.
--    Thus, they need to keep their info to some degree.
--    The amount of InactiveIs we keep as not-Junk is probably a tweakable parameter.
--    However, bounds of it can be estimated by looking at
--    the frequency of "revivals." vs "births"
--
--in order to be able to truncate the top of the ancestry tree safely while
--maintaining nice properties (same behavior as without truncating),
--we need to ensure that the truncated parts cannot become better than the
--currently active part of the pool. Also, they should be too far away from any active

data Pool = Pool {
  name :: String,
  iterations :: Int,
  maxSize :: Int,
  filteredSize :: Int,
  nextID :: Int, -- since we're a bit generous with these, we need to count genomes otherwise.
  --so some general statistics would be in order.
  genomes :: Tree Individual
} deriving (Show, Read)

-- problem-oriented fitness is a dead end.
-- factors to consider for CompoundFitness: (if applicable)
--  Fitness value of parent
--  Compilation rate of siblings
--  Compilation rate of parents' siblings
--  Fitness gain in last generation (fitself - fitparent)
--  Compilation rate of children
--  Number of generated children
-- All this information is readily available from the tree structure.
-- To get an overview, this data should be visualizable.
-- The following features are available:
--  Regarding children:
--    Mean and variance of available(i.e. computed without failure) fitness values, count and compilation rate.
--  Fit value of the individual
--  The features of children can always also be applied to grandchildren, etc.
-- When looking at a feature vector, it should be noted that most of the values are only partly explored.

--to figure out what turned out good and what turned out bad,
--  we need a true fitness function. We can't compute that on-line, and even
--  after the fact it's hard to do right.
--In short, properly deriving a heuristic to derive the importance of a
--  individual is hard. A mockup will do for now.
data FeatureVec = FeatureVec {
  id :: Int
, parentid :: Int
, fitness :: Float
, fitnessGainSinceParent :: Float
, compilationRate :: Ratio Integer
, children :: Int
, avgChildFit :: Float
} deriving (Show, Read)

main = do
  args <- getArgs
  processArgs args
  stopGlobalPool --stops the thread pool

--syntax: --start m n name --iterations i --no-output
--syntax: --load path --iterations 0
--syntax: --testrun
processArgs :: [String] -> IO ()
processArgs ["--testrun"] = testrun
processArgs ("--start" : max : min : name : "--iterations" : it : options) =
  initialPool (read max) (read min) name >>= runPool (read it) options
processArgs ("--load"  : path             : "--iterations" : it : options) =
  loadFromFile path                      >>= runPool (read it) options
processArgs _ = putStrLn "invalid operands"

output :: Pool -> IO()
output p = do
  writeFile (getUniqueName p ++ "-fitness") $ unlines $ map show $ sortBy (comparing getFitness) $ flatten $ genomes p
  writeFile (getUniqueName p ++ "-features") $ getFormattedFeatureDump p
  writeFile (getUniqueName p ++ ".dot") $ getDotFile p

--at some point, getDotFile needs to dynamically filter out irrelevant nodes.
--Particularly, any individual which does not have a live (grand, ..)parent is irrelevant.
getDotFile :: Pool -> String
getDotFile pool = unlines (["strict graph network{"] ++ edges ( genomes pool) ++ nodes ( genomes pool) ++ ["}"])
  where
    edges (Node a []) = []
    edges (Node a chdren) = concatMap edges chdren ++ (map (edge a . rootLabel) chdren)
    edge i1 i2 = 'n':(show $ GRPIndividual.getID i1) ++ "--" ++ 'n':(show $ GRPIndividual.getID i2) ++ ";"
    nodes gens = map (\gen -> 'n':((show $ GRPIndividual.getID gen) ++ "[label=" ++ (show $ show $ getFitness gen) ++ "];")) $ flatten gens

testrun :: IO()
testrun = do
  putStrLn "Starting test run."
  ip <- initialPool 100 3 "defaultTest"
  runPool 3 [] ip
  putStrLn "Done!"

runPool :: Int -> [String] -> Pool -> IO ()
runPool it options pool = do
  newPool <- iteratePool it options  pool
  writeFile (getUniqueName newPool) (show newPool)
  unless ("--no-output" `elem` options) $ output newPool

loadFromFile :: FilePath -> IO Pool
loadFromFile path = do
  str <- System.IO.Strict.readFile path
  return $ read str

initialPool :: Int -> Int -> String -> IO Pool
initialPool max min name = do
  src <- System.IO.Strict.readFile "./GRPSeed.hs"
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ unlines ( drop 2 $ lines src))
  generate "./GRPGenome0.hs"
  --This function does NOT write .stat to disk. This is done before calls to the executable.
  return (Pool name 0 max min 1 (Node (ActiveI 0 (Unchecked, 0.0) "./GRPGenome0") []))

getUniqueName :: Pool -> String
getUniqueName pool = name pool ++ "-" ++ show (iterations pool)

poolSummary :: Pool -> IO ()
poolSummary p = do
  let genP = flatten $ genomes p
  putStrLn "pool Summary:"
  putStrLn $ drawTree $ fmap show $ genomes p
  putStrLn "-+-+-+-+-+-+-+-+-+-+-+-+-+-"
  print $ filter (\i -> case i of
    ActiveI id (Compilation, f) p -> True;
    InactiveI id (Compilation, f) p -> True;
    _ -> False) genP  --(\i -> case i of JunkI _ -> False; InactiveI _ _ -> False; ActiveI Compilatio p -> ) genP
  putStrLn "---\n"

iteratePool :: Int -> [String] -> Pool -> IO Pool
iteratePool 0 options p = return p
iteratePool it options p = do
  --TODO: Change order of these operations.
  putStrLn "Starting iteration"
  ep <- evaluateFitness p
  let (fp, rmpaths) = filterPool ep
  cleanup rmpaths
  rp <- refillPool fp
  iteratePool (it-1) options rp{iterations = iterations rp +1}

getFormattedFeatureDump :: Pool -> String
getFormattedFeatureDump (Pool _ _ _ _ _ individuals) = format (iterateTZipper getFeatures $ fromTree individuals)
  where
    format featureVs = unlines $ map show $ catMaybes featureVs

iterateTZipper :: (TreePos Full Individual -> Maybe FeatureVec) -> TreePos Full Individual -> [Maybe FeatureVec]
iterateTZipper func z = func z : maybe [] (iterateTZipper func) (firstChild z) ++ maybe [] (iterateTZipper func) (next z)

--after adding id field for individuals, this seems unnecessary
getID :: TreePos Full Individual -> Int
getID loc = case label loc of
  ActiveI id (Compilation, _) name -> read $ filter (\c -> c `elem` ['0'..'9']) name
  InactiveI  id (Compilation, _) name -> read $ filter (\c -> c `elem` ['0'..'9']) name
  _ -> -1

--TODO: Refactor this mess:
getFeatures :: TreePos Full Individual -> Maybe FeatureVec
getFeatures zipperLoc = case label zipperLoc of
  ActiveI id (Compilation, fit) path -> Just (FeatureVec
      (Main.getID zipperLoc)
      (maybe (-1) Main.getID (parent zipperLoc))
      fit
      (maybe fit (\tp -> fit - (snd $ getFitness $ label tp)) (parent zipperLoc))
      (if (length offspringC + length offspringNC) == 0 then 0%1 else ((toInteger $ length offspringC) % (toInteger $ length offspringC + length offspringNC)))
      (length offspringC + length offspringNC)
      (if null offspringC then 0 else mean $ map (snd . getFitness) offspringC)
    )
    where (offspringC, offspringNC) =
            partition (\i -> (Compilation, -1.0 * (2^127)) <= getFitness i) $ map rootLabel $ subForest $ tree zipperLoc
  InactiveI id (Compilation, fit) _ -> Just (FeatureVec
      (Main.getID zipperLoc)
      (maybe (-1) Main.getID (parent zipperLoc))
      fit
      (maybe fit (\tp -> fit - (snd $ getFitness $ label tp)) (parent zipperLoc))
      (if (length offspringC + length offspringNC) == 0 then 0%1 else ((toInteger $ length offspringC) % (toInteger $ (length offspringC + length offspringNC))))
      (length offspringC + length offspringNC)
      (if null offspringC then 0 else mean $ map (snd . getFitness) offspringC)
    )
    where (offspringC, offspringNC) =
            partition (\i -> (Compilation, -1.0 * (2^127)) <= getFitness i) $ map rootLabel $ subForest $ tree zipperLoc
  _ -> Nothing

cleanup :: [String] -> IO()
cleanup [] = return ()
cleanup ("":xs) = cleanup xs
cleanup (x:xs) = do
  System.Directory.removeFile (x ++".hs")
  System.Directory.removeFile (x ++"hl.hs")
  cleanup xs

--cartesianProduct :: Pool -> IO Pool
--cartesianProduct (Pool max min nxt genomes) = do
--  let srcs = catMaybes $ path $ flatten genomes
--  let tickets = [nxt .. nxt -1 + length srcs * (length filter (\g -> case g of ActiveI _ _ -> True; _ -> False;) genomes)]

extractFromTreeContext :: (TreePos Full a -> b) -> Tree a -> Tree b
extractFromTreeContext f as = extractChildren f $ fromTree as
  where extractChildren f asZip = (Node (f asZip) [extractChildren f (fromJust $ childAt x asZip) | x <- [0..(length $ subForest $ tree asZip) - 1], isJust $ childAt x asZip])

getWeights :: Tree Individual -> Tree Float
getWeights individuals = fmap regress $ extractFromTreeContext getFeatures individuals
  where regress featrs = maybe 0 (\fs -> abs $ fitness fs + abs (fromRational (compilationRate fs) )) featrs

refillPool :: Pool -> IO Pool
refillPool (Pool name it max min nxt genomes) = do
  let size = length $ filter (\i -> case i of ActiveI _ _ _ -> True; _ -> False) $ flatten genomes
  let newGenomesCnt = max - size
  let tickets = [nxt .. nxt + newGenomesCnt - 1]
  --We definitely need to preprocess the fitness value
  -- - bare as they are, they're not really good for that purpose
  --negative weights are particularly bad. Thus: Absolute value, just to be sure.
  --TODO:
  let wtNodes = weightedAssign newGenomesCnt (fmap (\i -> case i of ActiveI _ (Compilation,f) _ -> abs f; _ -> 0) genomes)
  let ticketnodes = fmap (\x -> zip x (repeat Nothing) ) $ assignTickets wtNodes tickets
  newGenomes <- createAllChildren genomes ticketnodes
  return $ Pool name it max min (nxt + newGenomesCnt) newGenomes

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
    mkChild ind@(ActiveI parentID fit path) id srcCode = do
      writeFile (path ++ ".hs.stat") $show ind
      (code, out, err) <- readProcessWithExitCode "timeout"
        ["1s", path ++ "hl", "-e", srcCode ++ ".hs", "GRPGenome" ++ show id ++ ".hs"] ""
      System.Directory.removeFile (path ++ ".hs.stat") --state would be lost here
      if code == ExitSuccess
        then do
          generate ("GRPGenome" ++ show id ++ ".hs")
          return [Node (ActiveI id (Unchecked, 0.0) ("./GRPGenome" ++ show id)) []]
        else return []
  newElem <- mkChild (rootLabel $ tree loc) id srcCode -- rootlabel . tree == label ?
  return (modifyTree (\(Node a subnodes) -> Node a (newElem ++ subnodes)) loc)

filterPool :: Pool -> (Pool, [String])
filterPool (Pool name it max min nID population) =
  if length (filter (\i -> case i of JunkI _ _ -> False; _ -> True) $ flatten population) > min
  then (Pool name it max min nID updatedPopulation  , fileremovals)
  else (Pool name it max min nID updatedPopulation' , fileremovals)
  where
    (updPopAndFileRemovals) = fmap removeJunk population
    updatedPopulation' = fmap fst updPopAndFileRemovals
    fileremovals = flatten $ fmap snd updPopAndFileRemovals
    threshold          = getFitness $ (!!) (sortBy (flip compare) (flatten population)) (min - 1)
    --this labels individuals as inactive.
    updatedPopulation  = fmap (updateIndividual threshold) updatedPopulation'

--this function is going to become tricky later on. Right now, it's just a plain old
--map over the tree structure, but later we'll do a traversion of the neighborhood
--for every node considered.
--also, Fitness is likely to not cut it anymore later on as a type.
evaluateFitness :: Pool -> IO Pool
evaluateFitness pool = do
  let evalTree (Node a as) = do ind <-evalIndividual a; inds <- parallel $ map evalTree as; return (Node ind inds)
  genomes' <- evalTree (genomes pool)--this should be parallel
  return (pool{genomes = genomes'})

--a function :: (TreePos Individual Full -> Individual) ->
--  (TreePos Individual Full) -> (TreePos Individual Full)
--will come in handy
--  first argument gets a function to extract the relevant information from the neighborhood.
--  essentially maps from one Tree to the other
--this looks like a Functor instance for TreePos Full to some extent.

evalIndividual :: Individual -> IO Individual
evalIndividual (ActiveI id (Unchecked, val) path) = do
  src <- System.IO.Strict.readFile (path ++ ".hs")
  (fitVal, hints) <- computeFitness src (path ++ "hl.hs")
  if fitVal >= (Compilation, -1.0 * (2^127))
  then do
    writeFile (path ++ ".hs.stat") (show (ActiveI id fitVal path))
    (code, out, err) <- readProcessWithExitCode "timeout" ["1s", path ++ "hl", "-f", path ++ ".hs"] ""
    if code == ExitSuccess
    then do
      newFitStr <- System.IO.Strict.readFile (path ++ ".hs.stat")
      let (ActiveI idRet newFit pathRet) = read newFitStr
      System.Directory.removeFile (path ++".hs.stat")
      return (ActiveI idRet newFit pathRet)
    else do
      putStrLn "exit failure in eval"
      print (code, out, err)
      --System.Directory.removeFile (path ++".hs.stat")
      return (ActiveI id (Compilation, -1.0 * (2^120)) path)
  else return (ActiveI id fitVal path)
--Marking individuals as inactive shouldn't be done here.
evalIndividual indiv = return indiv
--do nothing if already evaluated or inactive/junk (which implies already evaluated)
