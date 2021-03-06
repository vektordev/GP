module GRPPool
( Pool (..)
, FeatureVec (..)
, LocalMaxState (..)
, State (..)
, loadFromFile
, initialPool
, runPool
, truncateP
, iterateTZipper
, getFeatures
, zipTreeWith
, getRegressedFeatures
, extractFromTreeContext
, activeRegression
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
import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)

import Data.Tree
import Data.Tree.Zipper
import Data.Maybe
import Data.Ord
import Data.List
import Data.Traversable
import Data.Ratio

import Debug.Trace

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.MSem
--as described here: http://stackoverflow.com/a/18898822

import Criterion.Measurement

--currently supports only one single root node. This can be changed later on.
--no State currently within Individual.
--
--TODO: Still not implemented:
-- +  cartesianProduct - rather easy, needs insertion of node
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
  nextID :: Int,
  genomes :: Tree Individual
} deriving (Show, Read)

-- problem-oriented fitness is a dead end.
-- factors to consider for CompoundFitness: (if applicable)
--  Compilation rate of siblings
--  Compilation rate of parents' siblings
-- All this information is readily available from the tree structure.
-- To get an overview, this data should be visualizable.
-- The following features are available:
--  Regarding children:
--    Mean and variance of available(i.e. computed without failure) fitness values, count and compilation rate.
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
, state :: State
, isLocalMax :: LocalMaxState
, generation :: Int
, fitness :: Float
, fitnessGainSinceParent :: Float
, compilationRate :: Ratio Integer
, compilationRateGain :: Ratio Integer
, children :: Int
, avgChildFit :: Float
} deriving (Show, Read)

data State = Active | Inactive | Junk deriving (Eq, Show, Read, Ord)
data LocalMaxState = LocalMax | Inherited | Good deriving (Eq, Show, Read, Ord)

maxNumberOfThreads = 8

--TODO: FeatureVec now has Active/Inactive/Junk.
--  Feature extraction should be agnostic of Individual state now,
--  as should be regression. Regression should be local-max agnostic.
--  Only when regression results are applied to shaping reproduction, these should be considered.
--    Has been implemented. Review required.

--TODO: Pool needs to respect InactiveI or ActiveI status. Relevant in refillPool, mostly

--TODO: Pool needs to make InactiveI/ActiveI decision dependant on regressed features.

output :: Pool -> IO()
output p = do
  writeFile (getUniqueName p ++ "-fitness") $ unlines $ map show $ sortBy (\(x,y) (x2,y2) -> compare (x, getFitness y) (x2, getFitness y2)) (zip (flatten $ getRegressedFeatures $ genomes p) (flatten $ genomes p))
  writeFile (getUniqueName p ++ "-features") $ getFormattedFeatureDump p
  writeFile (getUniqueName p ++ ".dot") $ getDotFile p
  writeFile (getUniqueName p ++ "-summary") $ getSummary p

getSummary :: Pool -> String
getSummary p = unlines [
  "IDs handed out = " ++ show (nextID p),
  "Individuals found: " ++ show (length $ flatten $ genomes p),
  "Compiling genomes: "++ show (length $ filter (\ind -> getFitness ind >= (UnknownCompilerError, 10^10)) $ flatten $ genomes p)
  ]

--at some point, getDotFile needs to dynamically filter out irrelevant nodes.
--Particularly, any individual which does not have a live (grand, ..)parent is irrelevant.
getDotFile :: Pool -> String
getDotFile pool = unlines (["strict graph network{"] ++ edges ( genomes pool) ++ nodes ( genomes pool) ++ ["}"])
  where
    edges (Node a []) = []
    edges (Node a chdren) = concatMap edges chdren ++ map (edge a . rootLabel) chdren
    edge i1 i2 = 'n':(show $ GRPIndividual.getID i1) ++ "--" ++ 'n':(show $ GRPIndividual.getID i2) ++ ";"
    nodes gens = map (\gen -> 'n':((show $ GRPIndividual.getID gen) ++ "[label=" ++ (show $ show $ getFitness gen) ++ "];")) $ flatten gens

runPool :: Int -> [String] -> Pool -> IO ()
runPool it options pool = do
  (code, out, err) <- readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
  writeFile (name pool ++ (show $ iterations pool) ++ "-" ++ show it ++ "-revision") out
  newPool <- iteratePool it options  pool
  writeFile (getUniqueName newPool) (show newPool)
  unless ("--no-output" `elem` options) $ output newPool

loadFromFile :: FilePath -> IO Pool
loadFromFile path = do
  str <- System.IO.Strict.readFile path
  return $ read str

initialPool :: Int -> Int -> String -> IO Pool
initialPool gain min name = do
  src <- System.IO.Strict.readFile "./GRPSeed.hs"
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ unlines ( drop 2 $ lines src))
  generate "./GRPGenome0.hs"
  --This function does NOT write .stat to disk. This is done before calls to the executable.
  evaluateFitness (Pool name 1 gain min 1 (Node (ActiveI 0 (Unchecked, 0.0) "./GRPGenome0") []))

truncateP :: String -> Pool -> IO Pool
truncateP newname pool = do
  let bestFit = maximum $ flatten $ getRegressedFeatures (genomes pool) :: Float
  let rootInd = ( fmap snd $ find (\x -> bestFit == fst x) (zip (flatten $ getRegressedFeatures (genomes pool)) $ flatten $ genomes pool) ) :: Maybe Individual
  let filepath = fromMaybe "" $ maybe Nothing path rootInd
  src <- System.IO.Strict.readFile (filepath ++ ".hs") --goes boom if any of the above fails.
  (code, out, err) <- readProcessWithExitCode "./soft-cleanup.sh" [] ""
  writeFile "./GRPGenome0.hs" ("--{-# LANGUAGE Safe #-}\nmodule GRPGenome0\n" ++ unlines ( drop 2 $ lines src))
  generate "./GRPGenome0.hs"
  return (Pool newname 1 (maxSize pool) (filteredSize pool) 1 (Node (ActiveI 0 (Unchecked, 0.0) "./GRPGenome0") []))

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

--TODO: timestamp each iteration start.
iteratePool :: Int -> [String] -> Pool -> IO Pool
iteratePool 0 options p = do
  putStrLn "iterations done."
  return p
iteratePool it options p = do
  initializeTime
  putStrLn ("Starting iteration " ++ show (iterations p))
  when (mod it 10 == 1) $ writeFile (getUniqueName p ++ "backup") (show p)
  putStrLn $ show $ length $ show p
  t0 <- getTime
  rp <- refillPool p
  t1 <- getTime
  ep <- evaluateFitness rp
  t2 <- getTime
  let (newPop, rmgenomes, recompilations, rmfiles) = filterPool (filteredSize ep) $ zipTreeWith (\a b -> (a,b)) (genomes ep) (getFilterWeights $genomes ep)
  t3 <- getTime
  putStrLn ("recompiling files: " ++ show recompilations)
  recompile recompilations
  t4 <- getTime
  unless ("--debug" `elem` options) $ cleanup rmgenomes --For Debug, do not clean up source code files. This allows for debugging of sneaky compiler errors.
  sequence $ fmap System.Directory.removeFile rmfiles   --Removing the executables is OK though.
  t5 <- getTime
  putStrLn (
    "Iteration ending. Profiling stats: tRefill: " ++ show (t1-t0) ++
    ", tEvalFit: " ++ show (t2-t1) ++
    ", tFilter: " ++ show (t3-t2) ++
    ", tRecompile: " ++ show (t4-t3) ++
    ", tCleanup" ++ show (t5-t4) ++ ".")
  iteratePool (it-1) options ep{iterations = iterations ep +1, genomes = newPop}

zipTreeWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTreeWith func (Node elA subforestA) (Node elB subforestB) = Node (func elA elB) $ zipWith (zipTreeWith func) subforestA subforestB
--can this not be expressed simpler? Why is there no typeclass for this?

recompile :: [String] -> IO()
recompile paths = do
  semaphor <- new maxNumberOfThreads
  _ <- mapConcurrently (\path -> with semaphor $ do (code, out, err) <- readProcessWithExitCode "ghc" [path] []; return ()) paths
  return ()

--TODO:
--Changes some labels around: keeps min Individuals active
--keeps another min Individuals Inactive
--labels the rest as JunkI
--Keep JunkI iff any of these is true:
--  It is less than X degrees of separation from the nearest InactiveI
--    X is the number of Degrees of separation that feature extraction considers
--  It is the only common ancestor of the entire [In]Active Population
--  It is a ancestor of only part of the Population
--This should leave: A tree where the [In]Active population from above persists,
--gets rated the same as before, has one and only one common ancestor and no
--unneeded Individuals
--removes all unneeded files
garbageCollection :: Pool -> IO Pool
garbageCollection pool = undefined

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
--TODO: Optimize this mess:
--  A lot of this will just calculate the same thing that we have already
--  calculated in the call for another node. This could be helped by first
--  mapping over the tree
--TODO: Also, this needn't be a Maybe Function. Just need to put the fitness and fitgain values to zero if it's not compiling
getFeatures :: TreePos Full Individual -> Maybe FeatureVec
getFeatures zipperLoc = case label zipperLoc of
  ActiveI id (Compilation, fit) _ -> Just $ computeFeatures Active
  InactiveI id (Compilation, fit) _ -> Just $ computeFeatures Inactive
  JunkI id (Compilation, fit) -> Just $ computeFeatures Junk
  _ -> Nothing
  where
    computeFeatures :: State -> FeatureVec
    computeFeatures state =
      (FeatureVec
        (GRPPool.getID zipperLoc)
        (maybe (-1) GRPPool.getID (parent zipperLoc))
        (state)
        (getIsLocalMax zipperLoc)
        --(if (length offspringC + length offspringNC) == 0 then False else ((toInteger $ length offspringC) % (toInteger $ (length offspringC + length offspringNC))) > 1 % 8)
        (getGeneration zipperLoc)
        (if state == Junk then 0 else snd $ getFitness $ label zipperLoc)
        (if state == Junk then 0 else maybe 0 (\tp -> (snd $ getFitness $ label zipperLoc) - (snd $ getFitness $ label tp)) (parent zipperLoc))
        (if (length offspringC + length offspringNC) == 0 then 0%1 else ((toInteger $ length offspringC) % (toInteger $ (length offspringC + length offspringNC))))
        (getCRateGain zipperLoc)
        (length offspringC + length offspringNC)
        (if null offspringC then 0 else mean $ map (snd . getFitness) offspringC)
      )
    (offspringC, offspringNC) =
      partition
        (\i -> (Compilation, -1.0 * (2^127)) <= getFitness i)
        $ map rootLabel $ subForest $ tree zipperLoc

getIsLocalMax loc = if maybe False localMaxRules (getFeatures loc) then LocalMax else if maybe False (\p -> Good /= getIsLocalMax p) $ parent loc then Inherited else Good

localMaxRules fv@(FeatureVec _ _ _ _ _ _ _ crate crategain chdr _) = (2 * fromRational crategain) > (c * (b ** (fromIntegral (- chdr)))) && crategain > 0.15
  where
    c = 1.63--c = 0.269
    b = 1.05--b = 1.02

getCRateGain :: TreePos Full Individual -> Ratio Integer
--getCRateGain loc = maybe (0%1) (\ploc -> (maybe (0%1) (compilationRate . getFeatures) loc) - maybe (0%1) (compilationRate . getFeatures) ploc) (parent loc)
getCRateGain loc = maybe (0%1) (\ploc -> (maybe (0%1) compilationRate $ getFeatures loc) - (maybe (0%1) compilationRate $ getFeatures ploc)) (parent loc)

getGeneration :: TreePos Full Individual -> Int
getGeneration loc = case parent loc of
  Nothing -> 1
  Just loc2 -> 1 + getGeneration loc2

cleanup :: [String] -> IO()
cleanup [] = return () --TODO: rm GRPGenome*.o GRPGEnome*.hi
cleanup ("":xs) = cleanup xs
cleanup (x:xs) = do
  System.Directory.removeFile (x ++".hs")
  System.Directory.removeFile (x ++"hl.hs")
  cleanup xs

extractFromTreeContext :: (TreePos Full a -> b) -> Tree a -> Tree b
extractFromTreeContext f as = extractChildren f $ fromTree as
  where extractChildren f asZip = (Node (f asZip) [extractChildren f (fromJust $ childAt x asZip) | x <- [0..(length $ subForest $ tree asZip) - 1], isJust $ childAt x asZip])

--from a Tree of Individuals, compute a tree of weights that can be used to choose active Individuals
getFilterWeights :: Tree Individual -> Tree Float
getFilterWeights individuals = fmap (maybe 0 adaptedRegression) $ extractFromTreeContext getFeatures individuals
  where
    adaptedRegression fv@(FeatureVec _ _ state localMax _ _ _ _ _ _ _) = if state == Junk || localMax /= Good then 0 else  activeRegression fv

--from a Tree of Individuals, compute a tree of weights that can be used to choose parents of this generation.
getRefillWeights :: Tree Individual -> Tree Float
getRefillWeights individuals = fmap (maybe 0 adaptedRegression) $ extractFromTreeContext getFeatures individuals
  where
    adaptedRegression fv@(FeatureVec _ _ state localMax _ _ _ _ _ _ _) = if state /= Active || localMax /= Good then 0 else  activeRegression fv

--from a Tree of Individuals, compute a Tree of regressed feature vectors. Keep in mind that this is not pre-processed yet, and should not be used as actual weight.
getRegressedFeatures :: Tree Individual -> Tree Float
getRegressedFeatures individuals = fmap (maybe 0 activeRegression) $ extractFromTreeContext getFeatures individuals

activeRegression (FeatureVec _ _ _ _ _ fit _ crate _ _ _) = fit * fromRational crate + 0.0001

--ignore compilation rate. Thus, we can try to fit to type check problem instead of overfitting ruthlessly.
regressFit :: FeatureVec -> Float
regressFit (FeatureVec _ _ _ localMax generation fit fitgain compilationrate cRateGain chdren avgchildfit) =
  abs fit --TODO evaluate fitgain as possible expansion.

regress :: FeatureVec -> Float
regress (FeatureVec _ _ state localMax generation fit fitgain compilationrate cRateGain chdren avgchildfit) =
  (abs fit + 10 * fitgain + abs (fromRational compilationrate)) * fromIntegral generation

--add 1 to numerator, so we remove some sampling bias. Respects fitness a tiny bit. Eliminates extremely high compilation rates, which indicate local max.
regressRateOnly :: FeatureVec -> Float
regressRateOnly (FeatureVec id _ state localMax generation fit fitgain compilationrate cRateGain chdren avgchildfit) =
  fromRational (((compilationrate * (fromIntegral chdren%1)) + 1) / ((fromIntegral chdren%1)+1)) + 0.02 * fit

cartesianProduct :: Pool -> IO Pool
cartesianProduct (Pool name  it max gain nxt genomes) = do
  --let size = length $ filter (\i -> case i of ActiveI _ _ _ -> True; _ -> False) $ flatten genomes
  let tickets = [nxt .. nxt + gain - 1]
  let wtNodes = weightedAssign gain (getRefillWeights genomes) :: Tree Int
  --let srcs = take newGenomesCnt $ mapMaybe path $ flatten genomes--TODO: sample according to wtNodes.
  let srcs = concat $ flatten $ fmap
                                    (\(ind, weight) -> maybe [] (replicate weight) (path ind))
                                    $ zipTreeWith (,) genomes wtNodes :: [String]
  let ticketnodes = fmap (\x -> zip x (repeat Nothing)) $ assignTickets wtNodes tickets :: Tree [(Int, Maybe String)]
  --probably best to use treeZipWith here... or so.
  --we can use foldl to associate every node in ticketNodes with a src.
  --srcs needs to be shuffled.
  --v v v Or something like that. replace every occurence of Nothing with a src
  let ticketnodes' = foldr (\srcs ticketNode -> ticketNode ) ticketnodes srcs
  newGenomes <- createAllChildren genomes ticketnodes
  return $ Pool name it max gain (nxt + gain) newGenomes
--TODO: Maybe an actual full cartesion Product is easier implemented, making it reasonably computable is done afterwards

refillPool :: Pool -> IO Pool
refillPool (Pool name it max gain nxt genomes) = do
  --let size = length $ filter (\i -> case i of ActiveI _ _ _ -> True; _ -> False) $ flatten genomes
  let newGenomesCnt = gain
  let tickets = [nxt .. nxt + newGenomesCnt - 1]
  let wtNodes = weightedAssign newGenomesCnt (getRefillWeights genomes) :: Tree Int
  rng <- newStdGen
  let sortedSrcAssignments = concat $ flatten $ zipTreeWith (\i ind -> replicate i $ path ind) wtNodes genomes :: [Maybe FilePath] --TODO
  let srcAssignments = shuffle' sortedSrcAssignments (length sortedSrcAssignments) rng
  let idNodes = assignTickets wtNodes tickets :: Tree [Int]
  let (ticketnodes, srcsLeftOver) = assignSourcesToIDs idNodes srcAssignments :: (Tree [(Int, Maybe FilePath)], [Maybe FilePath])
  newGenomes <- createAllChildren genomes ticketnodes
  return $ Pool name it max gain (nxt + newGenomesCnt) newGenomes

assignSourcesToIDs :: Tree [a] -> [b] -> (Tree [(a, b)], [b])
assignSourcesToIDs (Node nodeLst chdren) list = (Node elem' chdren', finalRest)--Node (zip nodeLst (take (length nodeLst) list)) $ foldr (\)
  where
    elem' = zip nodeLst (take (length nodeLst) list)
    rest = drop (length nodeLst) list
    (finalRest, chdren') = foldr foldInto (rest, []) chdren
    foldInto child (rest2, newchdr) = let (child', rest3) = assignSourcesToIDs child rest2 in (rest3, child' : newchdr)

--assigns tickets of type a to a Traversable of values of Ints, one ticket per value in each location.
assignTickets :: Traversable t => t Int -> [a] -> t [a]
assignTickets tree tickets =
  let
    (x, ticketNodes) =
      mapAccumR
        (\tickets weight ->
          if length tickets < weight
            then trace ("lacking " ++ show (weight - length tickets) ++ " tickets in assignTickets") ([], tickets)
            else (drop weight tickets, take weight tickets))
        tickets tree
  in if null x then ticketNodes else trace "Too many tickets in assignTickets" ticketNodes

createAllChildren :: Tree Individual -> Tree [(Int, Maybe FilePath)] -> IO (Tree Individual)
--general idea: To zipper, iterate. Recurse on leftmost child, add own children if applicable.
--recurse on right neighbor. If no right neighbor, return
createAllChildren iTree tTree = do
  upd <- zipperCreateAll (fromTree iTree)(fromTree tTree)
  putStrLn ("createAllChildren: Work done" ++ (show $ length $ show upd))
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
    Nothing -> trace "illegal state in GRPPool.hs:zipperCreateLocal" $ zipperCreateLocal tree ts --ignore this individual, because it's a JunkI
    Just x ->createChild tree i x >>= flip zipperCreateLocal ts
zipperCreateLocal tree ((i, Just path):ts) = createChild tree i path >>= flip zipperCreateLocal ts

--handles insertion into tree and failure of -e process.
createChild :: TreePos Full Individual -> Int -> FilePath -> IO(TreePos Full Individual)
createChild loc id srcCode = do
  let
    mkChild ind@(ActiveI parentID fit path) id srcCode = do
      writeFile (path ++ ".hs.stat") $show ind
      (code, out, err) <- readProcessWithExitCode "timeout"
        ["1s", path ++ "hl", "-e", path ++ ".hs", srcCode ++ ".hs", "GRPGenome" ++ show id ++ ".hs"] ""
      --System.Directory.removeFile (path ++ ".hs.stat") --state would be lost here
      if code == ExitSuccess
        then do
          generate ("GRPGenome" ++ show id ++ ".hs")
          return [Node (ActiveI id (Unchecked, 0.0) ("./GRPGenome" ++ show id)) []]
        else if code == ExitFailure 124
          then return [Node (JunkI id (RuntimeErrOnParent, 0.0)) []]
          else if "Non-exhaustive patterns in function" `isInfixOf` err || "head: empty list" `isInfixOf` err
            then return [Node (JunkI id (RuntimeErrOnParent, 0.0)) []]
            else trace ("runtime error was: " ++ err ++ "\nOutput was: " ++ out) return [Node (JunkI id (RuntimeErrOnParent, 0.0)) []]
  newElem <- mkChild (rootLabel $ tree loc) id srcCode -- rootlabel . tree == label ?
  return (modifyTree (\(Node a subnodes) -> Node a (newElem ++ subnodes)) loc)

--generates, from a lower bound on ActiveIs in the Pool and a Pool's genomes:
--  The updated Tree, with only min ActiveIs.
filterPool :: Int -> Tree (Individual, Float) -> (Tree Individual, [String], [String], [String])
filterPool min genomesAndFitness =
  --TODO: Does this filter need work?
  if length (filter (\i -> case i of JunkI _ _ -> False; _ -> True) $ flatten pop) < min
  then (fmap fst reducedPop, genomeRemovals, [], [])
  else (fmap (\(a,_,_)-> a) result, genomeRemovals, recompilations, fileremovals)
  where
    pop = fmap fst genomesAndFitness
    fit = fmap snd genomesAndFitness
    (updPopAndFileRemovals) = fmap (\(gen,fit) -> (removeJunk gen, fit)) genomesAndFitness
    genomeRemovals = catMaybes $ flatten $ fmap (snd . fst) updPopAndFileRemovals
    reducedPop :: Tree (Individual, Float)
    reducedPop = fmap (\((ugen, mFile), fit) -> (ugen, fit)) updPopAndFileRemovals
    threshold :: Float
    threshold = snd $ Data.List.last $ take (min) (sortBy (flip (comparing snd)) (flatten reducedPop))--TODO: Test
    result = fmap (\(ind, fit) -> if fit > threshold then setActive ind else setInactive ind) reducedPop
    recompilations = catMaybes $ flatten $ fmap (\(a,b,c) -> c) result
    fileremovals :: [String]
    fileremovals = (concat $ flatten $ fmap (\(a,b,c)-> b) result)

--this function is going to become tricky later on. Right now, it's just a plain old
--map over the tree structure, but later we'll do a traversion of the neighborhood
--for every node considered.
--also, Fitness is likely to not cut it anymore later on as a type.
evaluateFitness :: Pool -> IO Pool
evaluateFitness pool = do
  semaphor <- new maxNumberOfThreads
  genomes' <- mapConcurrently (with semaphor . evalIndividual) (genomes pool)--this should be parallel
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
      return (ActiveI id (Compilation, 0.0) path)
  else return (ActiveI id fitVal path)
--Marking individuals as inactive shouldn't be done here.
evalIndividual indiv = return indiv
--do nothing if already evaluated or inactive/junk (which implies already evaluated)
