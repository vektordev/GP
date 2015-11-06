module GRPGenerator
( generate
) where

import Data.Char

import System.IO.Strict(readFile)

headlessPath :: FilePath
headlessPath = "./GRPHeadless.hs"

--filePath: GenomeUniqueIdent.hs
--This function generates a matching hl.hs file that contains a main function and the relevant boilerplate.
generate :: FilePath -> IO ()
generate genomePath = do
  let moduleName = reverse $ takeWhile isAlphaNum $ drop 3 $ reverse genomePath
  let hlhsPath = moduleName ++ "hl.hs"
  fileContent <- System.IO.Strict.readFile headlessPath
  --get module name. Generate headless file for it: Drop and replace first line
  let newCode = unlines (("import " ++ moduleName ++ " as Genome") : tail ( lines fileContent))
  writeFile hlhsPath newCode
