module GRPTreeAnalytics where

import Data.Tree
import Debug.Trace
import Data.List
import Data.Function

import GRPStats

--Mostly chaotic, usually dead code. Should at some point generate a tree structure out of the pool. Or maybe I'll refactor the Pool to *be* a tree in the first place.
--In any case, a tree structure seems needed to figure out local maxima (a genome is part of a local maximum if none of it's offspring achieve a positive improvement with some significance)

type PoolTree = Tree AgentStats

treeThing = do
  str <- System.IO.Strict.readFile "./result88"
  let pool = read str
  putStrLn $ show $ length $ agents pool
  putStrLn $ show $ length $ oldAgents pool
  putStrLn $ drawTree $ fmap show $ mkAgentsTree $ ((agents pool) ++ (oldAgents pool))

processLogFile :: IO()
processLogFile = do
  content <- System.IO.Strict.readFile "result88"
  let
    pl = read content
    filterLambda ag = evaluatedChildren ag /= 0
    mapLambda ag = AgentStats (source ag) (getFitness ag) (if not $ null $ ancestry ag then [head $ ancestry ag] else []) (generation ag) (state ag) (fitnessImpactOnParent ag) (evaluatedChildren ag) (compiledChildren ag)
    pl' = Pool (maxSize pl) (filteredSize pl) (nextID pl) (map mapLambda $ filter filterLambda$ agents pl) (map mapLambda $ filter filterLambda $ oldAgents pl)
  --putStrLn $ show pl'
  --showPool pl'
  putStrLn $ drawTree $ fmap show $ toTree pl'

toTree :: Pool -> PoolTree
toTree (Pool _ _ _ new old) =
  let
    rootNode = Node (head $ filter (\ag -> source ag == "./GRPGenome0.hs") (new ++ old)) []
    allnodes = filter (/= rootNode) $ map (\ag -> Node ag [])(new ++ old)
  in
    trace ("root = " ++ show rootNode ) combineForestWithRoot allnodes rootNode

combineForestWithRoot :: [PoolTree] -> PoolTree -> PoolTree
combineForestWithRoot ns root =
  let (remainder, newRoot) = foldr (\node (rest, r) -> if Nothing == appendTree node r then (node:rest, r) else (rest, fromJust $ appendTree node r) ) ([], root) ns
  in if False then trace ("remainder after gen'ing tree: " ++ (show $ length remainder )) newRoot else trace ("remainder when gen'ing tree: " ++ (show $ length remainder )) combineForestWithRoot remainder newRoot

agent name dad = Node (AgentStats name (Unchecked, 0.0) [dad] 0 [] True 0 0) []

appendTree :: PoolTree -> PoolTree -> Maybe PoolTree
--append a to b - or try to, that is
appendTree a b =
  let
    zippedSubNodes = map (\subNode -> (rootLabel b , delete subNode (subForest b), subNode)) (subForest b)
    attempts =
      (catMaybes ((appendToNode a b)
      : map (\(root, rest, node) -> maybe Nothing (\x -> Just $ Node root (x : rest)) (appendTree a node) ) zippedSubNodes ) ) :: [PoolTree]
  in if null attempts then Nothing else Just $ head attempts
--  let attempts = catMaybes $ fmap (\parent -> appendToNode a parent) b

appendToNode child@(Node elem rest) parent@(Node elem' otherChildren) =
  if trace ("Trying to append " ++ source elem ++ " with parent " ++ (head $ ancestry elem) ++ " to " ++ source elem' ) (head $ ancestry elem) == (source elem')
  then trace "success" Just $ Node elem' (child:otherChildren)
  else Nothing

edgeList :: [(Int, Int)]
--assumes sorting
edgeList = [(0,1),(1,2),(2,3),(2,4)]
--data Tree a = Node [Tree] a | Leaf a

printThing tree =
  putStrLn $ drawTree $ fmap show tree

--mkTree takes a eq function on the node identifiers, a root node and a set of nodes.

getEdge :: AgentStats -> (String, String)
getEdge ag = (head $ ancestry ag, source ag)

ident :: AgentStats -> Int
ident agStats = read $ drop 3 $ reverse $ drop 11 $ source agStats

root ags = head $ filter (\ag -> source ag == "./GRPGenome0.hs") ags

mkAgentsTree ags =
  let
    sortedAgs = sortBy (on compare ident) ags
    -- sort by ascending ID. Since IDs are issued in chronological order, this way parents go in before their children.
    -- root ags = trace "evaling root" $ head $ filter (\ag -> trace "filter: " $ traceShowId $ source ag == "./GRPGenome0.hs") ags
  in
    mkTree (==) source (\ag -> (head $ ancestry ag, source ag)) (root ags) $ filter (\ag -> not $ null $ ancestry ag) sortedAgs

-- mkAgentsTree :: [AgentStats] -> Tree AgentStats
-- mkAgentsTree ags =
--   let
--     sorted = sortBy _ ags
--     root = source $ filter _ ags
--     stringTree = mkTree (==) root (map getEdge sorted)
--   in
--     _ --Stuff

--refac. IF:
--mkTree :: (b -> b -> Bool) -> (a -> b) -> (a -> (b,b)) -> a -> [a] -> Tree a
mkTree eq getId getEdge root nodes = trace "making tree" $ foldl (\tree node -> addEdge eq getId tree (getEdge node) node) (Node root []) nodes

--addEdge :: () () root edge elem
addEdge eq getId root@(Node label subNodes) (parent, child) element =
  if eq (getId label) parent
  then Node label (Node element [] : subNodes)
  else Node label (addEdgeToForest eq getId subNodes (parent, child) element)

--addEdgeToForest :: () () forest edge elem
addEdgeToForest eq getId forest edge element = map (\subtree -> addEdge eq getId subtree edge element) forest
--
-- mkTree :: (a -> a -> Bool) -> a -> [(a, a)] -> Tree a
-- mkTree eq root edges =
--   foldl (addEdge eq) (Node root []) edges
--
-- addEdge :: (a -> a -> Bool) -> Tree a -> (a,a) -> Tree a
-- addEdge eq (Node label subNodes) (parent, child) =
--   if eq parent label
--   then Node label (Node child [] : subNodes)
--   else Node label (addEdgeToForest eq subNodes (parent, child))
--
-- addEdgeToForest :: (a -> a -> Bool) -> [Tree a] -> (a,a) -> [Tree a]
-- addEdgeToForest eq forest edge = map (\subtree -> addEdge eq subtree edge ) forest
