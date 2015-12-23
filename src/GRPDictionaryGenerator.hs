module DictionaryGenerator
( mkDictionary
) where

import System.Process (readProcessWithExitCode)
import Debug.Trace
import System.Random
import Data.Char (isSpace)
import Data.List
import Data.List.Split
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension

data ProgramEnvironment = ProgramEnvironment [Instance] [Definition] deriving (Show, Read)

data Instance = Instance String deriving (Show, Read)
data Definition = Function String Constraint TypeVariable TypeVariable | Constant String TypeVariable deriving (Show, Read)
data TypeVariable = TypeVariable String | Type String deriving (Show, Read)
data Constraint = Constraint TypeClass TypeVariable deriving (Show, Read)
data TypeClass = TypeClass String deriving (Show, Read)

--acquiring instance info can be done through use of :info Type

mkDictionary :: IO ()
mkDictionary = do--"echo ":browse Prelude" | ghci"
  (code, out, err) <- readProcessWithExitCode "ghci" ["GRPSeed.hs"] ":browse! *GRPSeed"
  print code
  putStrLn "errors ahead"
  putStrLn err
  --putStrLn (show $ parseBrowseOutput out)
  parseResult <- parseBrowseOutput out
  writeFile "Dictionary.hs" (show parseResult)

parseBrowseOutput str = do
  writeFile "rawDict.hs" ((unlines $ map show failedP) ++ (unlines $ map show succP))
  putStrLn ("Failed: " ++ (show $ length failedP) ++ ", succeeded: " ++ (show $ length succP))
  return $ parseDeclWithMode mode modStr
  where
    modStr = drop 10 $ unlines $ drop 4 $ take (length (lines str) - 1) $lines str -- remuve the CLI prompt and all the other prefix/suffix info
    mode = defaultParseMode{baseLanguage = Haskell2010, extensions = extensions defaultParseMode ++ [EnableExtension ScopedTypeVariables]}
    groupedLns :: [[String]]
    groupedLns = drop 1 $ foldr (\line (grp:grps) -> if isSpace $ head line then (line:grp):grps else []:(line:grp):grps) [[]] $lines modStr
    rawAndParseData = map (\lnGrp -> (concat lnGrp, parseModuleWithMode mode $ unlines lnGrp)) groupedLns
    failedP = filter (\(grp, parse) -> case parse of ParseFailed _ _ -> True; otherwise -> False) rawAndParseData
    succP = filter (\(grp, parse) -> case parse of ParseFailed _ _ -> False; otherwise -> True) rawAndParseData
{-
parseBrowseOutput :: String -> ProgramEnvironment
parseBrowseOutput output =
  let
    filteredLns = drop 4 $ lines output
    preprocLns = init $ drop 10 (head filteredLns) : tail filteredLns
    groupedLns = foldr (\line (grp:grps) -> if isSpace $ head line then (line:grp):grps else []:(line:grp):grps) [[]] preprocLns
  in (trace $ show groupedLns) ProgramEnvironment [] []-}

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
