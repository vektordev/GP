module GRPDictionaryGenerator
( mkDictionary
) where

import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, renameFile)
import Debug.Trace
import System.Random
import Data.Char (isSpace)
import Data.List
import Data.List.Split
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import System.IO (openFile, hClose, IOMode(WriteMode), Handle)

import GhciEval

data ProgramEnvironment = ProgramEnvironment [Instance] [Definition] deriving (Show, Read)

data Instance = Instance String deriving (Show, Read)
data Definition = Function String Constraint TypeVariable TypeVariable | Constant String TypeVariable deriving (Show, Read)
data TypeVariable = TypeVariable String | Type String deriving (Show, Read)
data Constraint = Constraint TypeClass TypeVariable deriving (Show, Read)
data TypeClass = TypeClass String deriving (Show, Read)

--acquiring instance info can be done through use of :info Type

main = mkDictionary

mkDictionary :: IO ()
mkDictionary = do
  let (bang,star) = (True, True)
  (code, out, err) <- readProcessWithExitCode "ghci" ["GRPSeed.hs"] ((if bang then ":browse! " else ":browse ") ++ (if star then "*GRPSeed" else "GRPSeed"))
  print code
  unless (null err) $ putStrLn ("errors ahead: " ++ err)
  let wordsToLookUp = nub $ words out
  print wordsToLookUp
  handle <- openFile "Dictionary.hs~" WriteMode
  BS.hPut handle prefix
  addDeclarations True handle wordsToLookUp

  BS.hPut handle postfix
  hClose handle
  removeFile "Dictionary.hs"
  renameFile "Dictionary.hs~" "Dictionary.hs"
  --parseResult <- parseBrowseOutput out
  --writeFile "Dictionary.hs" (unlines $ map show $ concatMap extractDecls parseResult)

createFile :: [(String, String)] -> IO ()
createFile decl = BS.writeFile "Dictionary.hs" (createModule decl)

--TODO: Filter the ones that have IO in the result
addDeclarations :: Bool -> Handle -> [String] -> IO ()
addDeclarations isFirstInList handle [] = return ()
addDeclarations True handle (word:rest) = do
  result <- eval word
  unless (result == "") (BS.hPut handle ((BS.pack . show) (word, result)))
  addDeclarations (result == "") handle rest
addDeclarations False handle (word:rest) = do
  result <- eval word
  unless (result == "") (BS.hPut handle ((BS.pack ",\n  ") `BS.append` (BS.pack . show) (word, result)))
  addDeclarations False handle rest

createModule :: [(String, String)] -> BS.ByteString
createModule decls = BS.intercalate (BS.pack ",\n  ") $ map (BS.pack . show) decls

prefix = BS.pack "module Dictionary (\ndeclarations\n) where\n\n--Generated automatically using GRPDictionaryGenerator:mkDictionary - regenerate if out of date.\n\ndeclarations :: [(String, String)]\ndeclarations =\n  [\n  "
postfix = BS.pack "\n  ]\n"

extractDecls :: ParseResult Module -> [Decl]
extractDecls (ParseOk (Module _ _ _ _ _ _ decls)) = decls
extractDecls _ = []

parseBrowseOutput :: String -> IO [ParseResult Module]
parseBrowseOutput str = do
  writeFile "rawDict.hs" $ unlines (map show failedP ++ map show succP)
  putStrLn ("Failed: " ++ show (length failedP) ++ ", succeeded: " ++ show (length succP))
  return $ map snd succP --parseModuleWithMode mode modStr
  where
    modStr = drop 10 $ unlines $ drop 4 $ take (length (lines str) - 1) $lines str -- remove the CLI prompt and all the other prefix/suffix info
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
    [constraint, conversion] = if "=>" `isInfixOf` val then splitOn "=>" val else ["", val]
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
