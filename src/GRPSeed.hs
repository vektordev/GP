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
safeLines = 20
reprogram ( r1 : _ ) state _ = ( unwords code, state )
  where ( code , _ ) = pickNRandomly 100 lexems r1
initial = []
act rngs state ( PPI inp ) = ( PPO ( [ 0 ] , [ 0 ] ) , state )
act rngs state ( TCI inp ) = ( TCO "" , state )
