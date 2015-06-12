{-# LANGUAGE Safe #-}
module GRPSeed
( act
, reprogram
, initial
--, reinforcement
) where

--ALWAYS compile results of this as Safe

import System.Random
import Data.Maybe (isJust, fromJust)
import Data.List
import GRPCommon

reprogram :: [StdGen] -> State -> [String] -> (String, State)
act :: [StdGen] -> State -> Input -> (Output, State)
--reinforcement :: [StdGen] -> State -> Int -> String -> State
--initial :: State

{-
testrun = do
  text <- readFile "./GRPSeed.hs"
  rng <- newStdGen
  let (newcode, _) = reprogram [rng] [] [fromJust $ dropSafetyPrefix text]
  return newcode

t2 = do
  text <- readFile "./GRPSeed.hs"
  let t2 = fromJust $ dropSafetyPrefix text
  let preproc = unwords $ {- Here be mutate -}concat $ intersperse ["\n"] $ map words $ lines t2
  return preproc
-}

--Proposal for automatic generation of Seed from SeedHR:
--strict syntax: everything that would go into one line needs to be in a consecutive block of non-empty lines
--Everything that would go into different lines needs to be interspersed with newlines in between.
--Commends may only be -- at lines beginning in the actual code part.
--This way, automatic generation of GRPSeed should be as easy as removing comments,
--  replacing newlines with spaces between non-empty lines and then removing empty lines
--TODO: code the above and automatically execute it, also: Add GRPSeed to cleanup.sh

--The Danger Zone starts here. Keep the next line up to date:
safeLines = 44

act rngs state inp = ( ( take ( div ( length inp ) 2 ) inp, drop ( div ( length inp ) 2 ) inp ) , state )

reprogram ( r1 :  _ ) state ( source1 : _ ) =
  let
    candidates = map ( \ rng -> lexemlisttransform ( preproc source1 ) rng state ) ( infrg r1 )
  in ( head $ filter ( \ candidate -> candidate /= postproc ( preproc source1 ) ) ( map postproc $ filter ( \ x -> True ) candidates ) , state )

preproc str = intercalate [ "\n" ] $ map words $ lines str
postproc strs = rmlist ( \ x y -> x == '\n' && y == ' ' ) $ unwords strs

infrg rg = let ( x , y ) = split rg in x : infrg y

rmlist predicate ( x : y : ys ) = if predicate x y then rmlist predicate ( x : ys ) else x : rmlist predicate ( y : ys )
rmlist a xs = xs

initial = [ 10000000 , 20000000 ]

lexemlisttransform :: [ String ] -> StdGen -> State -> [ String ]
lexemlisttransform [] rng state = []
lexemlisttransform ( lex : lst ) rng state =
  let ( decision , rng2 ) = next rng :: ( Int , StdGen )
  in
    if decision < 10000000
    then let ( n , rng3 ) = next rng2 in ( lexems !! mod n ( length lexems ) ) :  lex : lexemlisttransform lst rng3 state
    else
      if decision < 20000000
      then lexemlisttransform lst rng2 state
      else lex : lexemlisttransform lst rng2 state
