module GRPIndividual
( Individual(JunkI, ActiveI, InactiveI)
, getFitness
, setFitness
, updateIndividual
, removeJunk
) where

import GRPFitness
import Data.Ord

--newly generated individuals are labeled ActiveI,
--JunkI is for individuals who will never become active again
--InactiveI is for individuals who can currently not compete.
data Individual = JunkI Fitness | ActiveI Fitness FilePath | InactiveI Fitness FilePath deriving (Show, Read, Eq)

instance Ord Individual where
  compare = comparing getFitness

getFitness (JunkI f)       = f
getFitness (ActiveI f _)   = f
getFitness (InactiveI f _) = f

setFitness :: Individual -> Fitness -> Individual
setFitness (JunkI f) f' = JunkI f'
setFitness (ActiveI f p) f' = ActiveI f' p
setFitness (InactiveI f p) f' = InactiveI f' p

updateIndividual :: Fitness -> Individual -> Individual
--from a fitness threshold, determine if a individual is supposed to be kept active
updateIndividual thres (JunkI     f)      = JunkI f
updateIndividual thres (ActiveI   f path) = if f >= thres then ActiveI f path else InactiveI f path
updateIndividual thres (InactiveI f path) = if f >= thres then ActiveI f path else InactiveI f path

removeJunk :: Individual -> Individual
removeJunk ind@(ActiveI (Compilation, f2) p) = ind
removeJunk ind@(InactiveI (Compilation, f2) p) = ind
removeJunk ind = JunkI $ getFitness ind
