module GRPStats
( AgentStats(AgentStats)
, createAncestry
, findCommonAncestor
--, setState
, source
, getFitness
, ancestry
, generation
, state
) where

import GRPFitness
import Data.Eq
import GRPCommon

data AgentStats = AgentStats{
  source :: FilePath,
  getFitness :: Fitness,
  --Maybe add Maybe Deep Fitness value? If a deep value has been computed, it is likely incomparable to shallow ones.
  --Fitness needs to store a lot more data, it seems
  ancestry :: [FilePath],--Does not contain this agent's filepath.
  generation :: Int,
  --possibly unneeded - should be length ancestry +1
  --this is agnostic of pool generations. So a generation 1 genome that produces a mutation 2 pool-generations after it's introduction will create a generation 2 genome.
  state :: State
  --wasCompiled :: Bool
  --has the agent already been compiled? Not required given the current approach.
} deriving (Show, Read)

instance Eq AgentStats where
  a1 == a2 = getFitness a1 == getFitness a2

instance Ord AgentStats where
  compare a1 a2 = compare (getFitness a1) (getFitness a2)

--TODO: Find common ancestor
--The relevant segment of the search tree is a Y-shaped graph segment - the central node being the common ancestor.
--The length of the 3 segments in relation to each other displays genetic similarity.
--Result: Trunk (common part) length - the difference can be calculated using generation ag
findCommonAncestor :: AgentStats -> AgentStats -> Int
findCommonAncestor (AgentStats s1 _ a1 g1 _) (AgentStats s2 _ a2 g2 _) =
  let fn (a1:a1s) (a2:a2s) = if a2 == a1 then length (a1:a1s) else fn a1s a2s
  in (if s1 == s2 then 1 else 0) + if g1 > g2 then fn (drop (g1-g2) a1) a2 else fn a1 (drop (g2-g1) a2)

createAncestry :: AgentStats -> [FilePath]
createAncestry ag = source ag : ancestry ag

toFile :: AgentStats -> (FilePath, String)
toFile ag = (source ag ++ ".stat", show ag)

getSource :: AgentStats -> IO String
getSource ag = readFile $ (source ag ++ ".hs")

fromFile :: String -> AgentStats
fromFile = read
