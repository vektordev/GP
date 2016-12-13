module GRPCommon
( ite
, mklst
, mkNullLst
, mkSpanningList
, mkPair
, brClose
, brOpen
, space
, fourTuple
, fst4
, snd4
, third4
, fourth4
, mkChar
, functions
, programAtoms
, ws1
, weightsum
, pick
, pickWord
, choose
, State
, Input (..)
, Output (..)
, PartitioningProblemInput
, PartitioningProblemOutput
, lexems
, pickRandomly
, pickNRandomly
) where

import System.Random
import Dictionary
import Data.List

{-
  This file contains some basic functions that are supposed to be used by the code generator
  Most of it is outdated, as it originates from an earler model of the code generator
  Even the up-to-date part could use work and maybe additional data.
  For example, it seems vital that the code generator has access to some statistical data about
  the words it's supposed to write code from. This includes, for example, likelihood of said word appearing in code.
  While said likelihood could be a genetic parameter, it might as well be extracted from somewhere else and taken as domain knowledge.
  The question of "is this domain knowledge I'm including part of the code gen or a library for the code gen?" is a common theme.
-}

--TODO: This whole file is deprecated and needs refactoring.

--TODO: Use a hashmap / assocmap like structure for memory.

--these need domain specific modeling, maybe. Output and Input should be typed according to the problem domain.
type State = [Int]
data Input = PPI PartitioningProblemInput | TCI TCInput | WarningsShutTheTrap deriving (Show, Read)
data Output = PPO PartitioningProblemOutput | TCO TCOutput | NotImplemented deriving (Show, Read)

type PartitioningProblemInput = [Int]
type PartitioningProblemOutput = ([Int], [Int])

type TCInput = [String]
type TCOutput = String

ite a b c = if a then b else c

mklst a = [a]

mkNullLst = []

mkSpanningList a b = [a..b]

mkPair a b = (a,b)
--starting here, these thingies need addition to functions
brClose = " ) "
brOpen = " ( "
space = " "

threeTuple a b c = (a,b,c)
fst3 (a,b,c) = a
snd3 (a,b,c) = b
third3 (a,b,c) = c

fourTuple a b c d = (a,b,c,d)
fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
third4 (a,b,c,d) = c
fourth4 (a,b,c,d) = d
--end

pickRandomly :: [a] -> StdGen -> (a, StdGen)
pickRandomly elems rng = (elems !! (ix `mod` length elems), rng2)
  where (ix, rng2) = next rng

pickNRandomly :: Int -> [a] -> StdGen -> ([a], StdGen)
pickNRandomly 1 lst rng = ([pick], rng')
  where (pick, rng') = pickRandomly lst rng
pickNRandomly x lst rng = if x > 0 then ((pick:rest),rng') else ([], rng)
  where
    (pick, rng'') = pickRandomly lst rng
    (rest, rng') = pickNRandomly (x-1) lst rng''

--TODO: Only allow printable chars here? \t, \n, ' ', symbols, digits and characters?
mkChar = (!!) ['\0'..'\127']

--(name, prefix function parameters, other parameters, weighting)
--Eventually, this is to be replaces by (String, TypeSignature, Int).
--This will be done in order to allow type-inferred expression building.
functions :: [(String, Int, Int, Int)]
functions = [
--  ("", 0, , 1),
--    ("ite", 0, 3, 1),
  ("mklst", 0, 1, 0),
  ("mkNullLst", 0, 0, 1),
--    ("mkSpanningList", 0, 2, 1),
  ("mkPair", 0, 2, 1),
  ("fst", 0, 1, 1),
  ("snd", 0, 1, 1),
  ("next", 0, 1, 1),
  ("rndgen", 0, 1, 1), --This one doesn't fly
  ("mkChar", 0, 1, 1),
--    ("(:)", 0, 2, 1),
  ("(++)", 0, 2, 1),
  ("source", 0, 0, 1),
--    ("head", 0, 1, 1),
--    ("last", 0, 1, 1),
--    ("tail", 0, 1, 1),
--    ("init", 0, 1, 1),
--    ("length", 0, 1, 1),
--    ("null", 0, 1, 1),
--    ("(==)", 0, 2, 1),
--    ("map", 1, 1, 0),
--    ("filter", 1, 1, 0),
--    ("foldr", 1, 2, 0),
--    ("foldl", 1, 2, 0),
--    ("(+)", 0, 2, 1),
--    ("(-)", 0, 2, 1),
--    ("(*)", 0, 2, 1),
--    ("(^)", 0, 2, 1),
--    ("mod", 0, 2, 1),
--    ("(<=)", 0, 2, 1),
--    ("(<)", 0, 2, 1),
--    ("(>)", 0, 2, 1),
--    ("(>=)", 0, 2, 1),
  ("1", 0, 0, 4),
  ("2", 0, 0, 2)
--    ("3", 0, 0, 4),
--    ("4", 0, 0, 2),
--    ("5", 0, 0, 2),
--    ("6", 0, 0, 2),
--    ("7", 0, 0, 1),
--    ("8", 0, 0, 1)
  --How to generate primitive numbers?
  ]

programAtoms = [
  ("code", "", 1),
  ("rndgen", "", 1),
  ("source", "", 1),
  ("xs", "", 1),
  ("r1", "", 1),
  ("r2", "", 1),
  ("x1", "", 1),
  ("x2", "", 1),
  ("create", "", 1),
  ("let", "", 1),
  ("in", "", 1),
  ("if", "", 1),
  ("then", "", 1),
  ("else", "", 1),
  ("(", "", 1),
  (")", "", 1),
  ("[", "", 1),
  ("]", "", 1),
  (",", "", 1),
  (":", "", 1),
  ("\n", "", 1),
  ("next", "", 1),
  ("1", "", 1),
  ("2", "", 1),
  ("+", "", 1),
  ("-", "", 1),
  ("*", "", 1),
  ("/", "", 1),
  ("fst", "", 1),
  ("snd", "", 1),
  ("head", "", 1),
  ("tail", "", 1),
  ("mod", "", 1),
  ("pick", "", 1),
  ("++", "", 1)
  ]

--getUnindexedWords = do
--  src <- readFile "./GRPSeedhr.hs"
--  putStrLn $ unlines $ nub $ sort $ words $ unlines $ drop 44 $ lines src;
--  putStrLn $ show ((nub $ words $ unlines $ drop 44 $ lines src) \\ lexems)

--do src <- readFile "./GRPSeedhr.hs" ; putStrLn $ unlines $ nub $ sort $ words $ unlines $ drop 44 $ lines src
stuff = [
  "(",
  ")",
  "[",
  "]",
  "[]",
  "{",
  "}",
  ",",
  ":",
  ";",
  "!!",
  "$",
  "->",
  "<-",
  --"do", crappy idea - lotsa infinite loops
  "\n",
  "\"\\n\"",
  "True",
  "False",
  "&&",
  "||",
  "<",
  "<=",
  "==",
  "/=",
  "=",
  "not",
  "\\",
  "_",
  "+",
  "-",
  "*",
  "^",
  "div",
  "1",
  "2",
  "10000000",
  "act",
  "reprogram",
  "initial",
  --mark
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p6",
  --mark2
  "words",
  "unwords",
  "lines",
  "unlines",
  "let",
  "in",
  "if",
  "then",
  "else",
  "fst",
  "snd",
  "head",
  "tail",
  "drop",
  "take",
  "length",
  "map",
  "foldl",
  "foldr",
  "filter",
  "split",
  "mod",
  "++",
  "fst",
  "snd",
  "intercalate",
  "(++)",
  --Not to be included in the end
  "lexemlisttransform",
  "preproc",
  "postproc",
  "rmlist"
{-  -- Absolute trash: names of variables etc. These should be refactored to limit the total vocabulary.
  "rngs",
  "state",
  "inp",
  "source1",
  "candidates",
  "rng",
  "infrg",
  "candidate",
  "x",
  "str",
  "strs",
  "y",
  "'\\n'",
  "rg",
  "inp",
  "rng3",
  "lexems",
  "n",
  "Int",
  "next",
  "decision",
  "lex",
  "inp,",
  "ys",
  "a",
  "::",
  "lst",
  "predicate",
  "'",
  "String",
  "StdGen",
  "State",
  "rng2"-}
  ]

--lexems = map fst3 programAtoms ++ map fst4 functions

ppWords = [
  "foldl",
  "foldr",
  "map",
  "filter",
  "(",
  ")",
  "\\",
  "+",
  "-",
  "*",
  "div",
  "==",
  "<=",
  "/=",
  "not",
  "act",
  "func",
  "p1",
  "p2",
  "p3",
  "p4",
  "let",
  "in",
  "->",
  "$",
  "."
  ]
lexems = nub (ppWords ++ stuff ++ map fst4 functions ++ map fst3 programAtoms)

ws1 [] = 0
ws1 ((_,_,x):xs) = x + ws1 xs

pick n =
  let func ( (a,b, x): xs) n2 = if n2 > x then func (if null xs then programAtoms else xs ) (n2-x) else (a,b,x)
  in func programAtoms (mod n (ws1 programAtoms) )

pickWord n =
  let (word, _, _) = pick n
  in word

weightsum [] = 0
weightsum ((_,_,_,weight): xs) = weight + weightsum xs

choose ((a,b,c,d):xs) n = if n > d then choose xs (n-d) else (a,b,c,d)
