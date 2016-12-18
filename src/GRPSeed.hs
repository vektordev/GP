--{-# LANGUAGE Safe #-}
module GRPSeed
( act
, reprogram
, initial
--, reinforcement
) where

--Best to compile results of this as Safe if the code gen has sufficient freedom.

import System.Random
import Data.Maybe (isJust, fromJust)
import Data.List
import GRPCommon
import Language.Haskell.Exts.Parser -- limit those imports.
import Language.Haskell.Exts.Syntax

reprogram :: [StdGen] -> State -> [String] -> (String, State)
act :: [StdGen] -> State -> Input -> (Output, State)
--reinforcement :: [StdGen] -> State -> Int -> String -> State
--initial :: State

{-
testrun = do
  text <- System.IO.Strict.readFile "./GRPSeed.hs"
  rng <- newStdGen
  let (newcode, _) = reprogram [rng] [] [fromJust $ dropSafetyPrefix text]
  return newcode

t2 = do
  text <- System.IO.Strict.readFile "./GRPSeed.hs"
  let t2 = fromJust $ dropSafetyPrefix text
  let preproc = unwords $ {- Here be mutate -}concat $ intersperse ["\n"] $ map words $ lines t2
  return preproc
-}

--TODO: Refactor the Seed function. As of now, we apparently do not need to stick to the restricted syntax.
--thus, line endings, type annotations etc. are possible. Use the syntax checker to preprocess everything.
--Preferably train the type checker to work on AST-level.
--The Danger Zone starts here. Keep the next line up to date:
safeLines = 41
reprogram ( r1 : _ ) state ( source1 : _ ) = let candidates = map ( \ rng -> lexemlisttransform ( preproc source1 ) rng state ) ( infrg r1 ) in (head $ filter ( \ candidate -> ( candidate /= postproc ( preproc source1 ) ) && ( parseable candidate ) ) (map postproc $ filter (\x -> True) candidates), state )
parseable str = let result = parseModule str in wasSuccess result
wasSuccess ( ParseFailed _ _ ) = False
wasSuccess ( ParseOk _ ) = True
typechecks str = case parseModule str of ParseOk ast -> typecheck ast ; otherwise -> False
typecheck ast = True
preproc str = intercalate ["\n"] $ map words $ lines str
postproc strs = rmlist ( \ x y -> x == '\n' && y == ' ' ) $ unwords strs
infrg rg = let ( x , y ) = split rg in x : infrg y
rmlist predicate ( x : y : ys ) = if predicate x y then rmlist predicate ( x : ys ) else x : rmlist predicate ( y : ys )
rmlist a xs = xs
initial = [ 10000000 , 20000000 ]
lexemlisttransform [] rng state = []
lexemlisttransform ( lex : lst ) rng state = let ( decision , rng2 ) = next rng :: ( Int , StdGen ) in if decision < ( head initial ) then let ( n , rng3 ) = next rng2 in ( lexems !! ( mod n $ length lexems ) ) : lex : ( lexemlisttransform lst rng3 state) else if decision < ( last initial ) then lexemlisttransform lst rng2 state else lex : ( lexemlisttransform lst rng2 state)
act rngs state ( PPI inp ) = ( PPO ( [ 0 ] , [ 0 ] ) , state )
act rngs state ( TCI inp ) = (TCO ( "" ) , state )
