--{-# LANGUAGE Safe #-}
module GRPParserSeed
( act
, reprogram
, initial
--, reinforcement
) where

--Ideally, we would compile results of this as safe.

import System.Random
import Data.Maybe (isJust, fromJust)
import Data.List
import GRPCommon

--limit the imports on this one. This is an unsafe module, so we can't trust it.
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

--TODO: rephrase reprogram in terms of act and let [String] be a subset of Input and String be a subset of Output?
reprogram :: [StdGen] -> State -> [String] -> (String, State)
act :: [StdGen] -> State -> Input -> (Output, State)
--reinforcement :: [StdGen] -> State -> Int -> String -> State
--initial :: State

--The Danger Zone starts here. Keep the next line up to date:
safeLines = 26

data TypeCheckResult = TypeErr | NotChecked | Result CustomType

type DeclInfo = [(String, CustomType)]

data CustomType = Function CustomType CustomType | Tuple CustomType CustomType | List CustomType | Unit

act rngs state inp = ( ( take ( div ( length inp ) 2 ) inp, drop ( div ( length inp ) 2 ) inp ) , state )

reprogram ( r1 :  _ ) state ( source1 : _ ) = undefined

infrg rg = let ( x , y ) = split rg in x : infrg y

mutateExp :: Float -> StdGen -> Exp -> Exp
mutateExp = undefined

parseEx :: String -> Maybe Exp
parseEx str = case parseExp str of
  ParseFailed _ _  -> Nothing
  ParseOk ex -> Just ex

followTheType :: DeclInfo -> CustomType -> Exp
followTheType = undefined

typechecks :: DeclInfo -> Exp -> TypeCheckResult
typechecks _ = NotChecked
