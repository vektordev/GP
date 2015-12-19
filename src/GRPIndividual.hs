module GRPIndividual
( Individual(JunkI, ActiveI, InactiveI)
, getFitness
, path
, setFitness
, updateIndividual
, removeJunk
, getID
, setActive
, setInactive
) where

import GRPFitness
import Data.Ord

--newly generated individuals are labeled ActiveI,
--JunkI is for individuals who will never become active again
--InactiveI is for individuals who can currently not compete.
data Individual = JunkI Int Fitness
                | ActiveI Int Fitness FilePath
                | InactiveI Int Fitness FilePath
                deriving (Show, Read, Eq)

instance Ord Individual where
  compare = comparing getFitness

getFitness (JunkI _ f)       = f
getFitness (ActiveI _ f _)   = f
getFitness (InactiveI _ f _) = f

getID (JunkI i _) = i
getID (ActiveI i _ _) = i
getID (InactiveI i _ _) = i

path (JunkI _ f) = Nothing
path (ActiveI _ f p) = Just p
path (InactiveI _ f p) = Just p

setFitness :: Individual -> Fitness -> Individual
setFitness (JunkI i f) f' = JunkI i f'
setFitness (ActiveI i f p) f' = ActiveI i f' p
setFitness (InactiveI i f p) f' = InactiveI i f' p

updateIndividual :: Fitness -> Individual -> Individual
--from a fitness threshold, determine if a individual is supposed to be kept active
updateIndividual thres (JunkI     i f)      = JunkI i f
updateIndividual thres (ActiveI   i f path) = if f >= thres then ActiveI i f path else InactiveI i f path
updateIndividual thres (InactiveI i f path) = if f >= thres then ActiveI i f path else InactiveI i f path

setActive :: Individual -> (Individual, [FilePath], Maybe FilePath)
setActive (InactiveI i f path) = (ActiveI i f path, [], Just $ path ++ "hl.hs")
setActive ind = (ind, [], Nothing)

setInactive :: Individual -> (Individual, [FilePath], Maybe FilePath)
setInactive (ActiveI i f path) = (InactiveI i f path, [path ++ "hl"], Nothing)
setInactive ind = (ind, [], Nothing)

removeJunk :: Individual -> (Individual, Maybe String)
removeJunk ind@(ActiveI i (Compilation, f2) p) = (ind, Nothing)
removeJunk ind@(InactiveI i (Compilation, f2) p) = (ind, Nothing)
removeJunk ind@(JunkI i p) = (ind, Nothing)
removeJunk ind@(ActiveI i (_, f) p) = (JunkI i $ getFitness ind, Just p)
