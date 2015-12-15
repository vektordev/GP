module DictionaryGenerator
( mkDictionary
) where

import System.Process (readProcessWithExitCode)
import Debug.Trace
import System.Random
import Data.Char (isSpace)
import Data.List
import Data.List.Split

data ProgramEnvironment = ProgramEnvironment [Instance] [Definition] deriving (Show, Read)

data Instance = Instance String deriving (Show, Read)
data Definition = Function String Constraint TypeVariable TypeVariable | Constant String TypeVariable deriving (Show, Read)
data TypeVariable = TypeVariable String | Type String deriving (Show, Read)
data Constraint = Constraint TypeClass TypeVariable deriving (Show, Read)
data TypeClass = TypeClass String deriving (Show, Read)

--acquiring instance info can be done through use of :info Type

mkDictionary :: String -> IO ()
mkDictionary str = do--"echo ":browse Prelude" | ghci"
  (code, out, err) <- readProcessWithExitCode "ghci" ["GRPSeed.hs"] ":browse! *GRPSeed"
  print code
  putStrLn out
  putStrLn "errors ahead"
  putStrLn err
  putStrLn (show $ parseBrowseOutput out)
  writeFile "Dictionary" (show $ parseBrowseOutput out)

parseBrowseOutput :: String -> ProgramEnvironment
parseBrowseOutput output =
  let
    filteredLns = drop 4 $ lines output
    preprocLns = init $ drop 10 (head filteredLns) : tail filteredLns
    groupedLns = foldr (\line (grp:grps) -> if isSpace $ head line then (line:grp):grps else []:(line:grp):grps) [[]] preprocLns
  in (trace $ show groupedLns) ProgramEnvironment [] []

parseFunction :: String -> Definition
parseFunction str =
  let
    [name, val] = splitOn "::" str
    [constraint, conversion] = if isInfixOf "=>" val then splitOn "=>" val else ["", val]
    (from,to) = splitOnArrow [] [] conversion
  in
    Function name (parseConstraint constraint) (parseTypeVariable from) $ parseTypeVariable to

splitOnArrow :: String -> [Char] -> String -> (String, String)
splitOnArrow prefix [] ('-':'>':xs) = (prefix,xs)
splitOnArrow prefix x ('(':xs) = splitOnArrow (prefix ++ ['(']) ('(':x) xs
splitOnArrow prefix ('(':x) (')':xs) = splitOnArrow (prefix ++ [')']) x xs
splitOnArrow prefix x (c:xs) = splitOnArrow (prefix ++ [c]) x xs

parseConstraint :: String -> Constraint
parseConstraint str =
  let
    [tclass, var] = words str
  in Constraint (TypeClass tclass) $ Type var

parseTypeVariable :: String -> TypeVariable
parseTypeVariable s = TypeVariable s
