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
, Input
, Output
, lexems
) where

--TODO: This whole file is deprecated and needs refactoring.

--these need domain specific modeling, maybe. Output and Input should be typed according to the problem domain.
type State = [Int]
type Output = ([Int], [Int])
type Input = [Int]


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


--TODO: Only allow printable chars here? \t, \n, ' ', symbols, digits and characters?
mkChar n = (!!) ['\0'..'\127'] n

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

--TODO: add type declaration here  - use Int rather than default Integer....?
programAtoms = [
  (" code", "", 1),
  (" rndgen", "", 1),
  (" source", "", 1),
  (" xs", "", 1),
  (" r1", "", 1),
  (" r2", "", 1),
  (" x1", "", 1),
  (" x2", "", 1),
  (" create", "", 1),
  (" let", "", 1),
  (" in", "", 1),
  (" if", "", 1),
  (" then", "", 1),
  (" else", "", 1),
  (" (", "", 1),
  (" )", "", 1),
  (" [", "", 1),
  (" ]", "", 1),
  (" ,", "", 1),
  (" :", "", 1),
  (" \n", "", 1),
  (" next", "", 1),
  (" 1", "", 1),
  (" 2", "", 1),
  (" +", "", 1),
  (" -", "", 1),
  (" *", "", 1),
  (" /", "", 1),
  (" fst", "", 1),
  (" snd", "", 1),
  (" head", "", 1),
  (" tail", "", 1),
  (" mod", "", 1),
  (" pick", "", 1),
  (" ++", "", 1)
  ]

lexems = map fst3 programAtoms ++ map fst4 functions

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
