module GRPGenerator
( generate
) where

import Data.Char

import System.IO.Strict(readFile)

--This file will evaluate source code once the Plugin library is included in here.
headlessPath :: FilePath
headlessPath = "./GRPHeadless.hs"

--filePath: GenomeUniqueIdent.hs
--This function generates a hl.hs file
generate :: FilePath -> IO ()
generate genomePath = do
  --putStrLn "generating"
  --file <- readFile "GRPGenome0hl.hs"
  --putStrLn file
  let moduleName = reverse $ takeWhile isAlphaNum $ drop 3 $ reverse genomePath
  let hlhsPath = moduleName ++ "hl.hs"
  --putStrLn hlhsPath
  fileContent <- System.IO.Strict.readFile headlessPath
  --get module name. Generate headless file for it: Drop and replace first line
  let newCode = unlines (("import " ++ moduleName ++ " as Genome") : tail ( lines fileContent))
  --file2 <- readFile "GRPGenome0hl.hs"
  --putStrLn file2
  writeFile hlhsPath newCode
