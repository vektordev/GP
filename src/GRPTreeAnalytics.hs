module GRPTreeAnalytics where

import Data.Tree
import Debug.Trace
import Data.List
import Data.Function

import GRPStats

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
