module Main
(
  main
) where

import System.IO
import System.Environment (getArgs)

import GRPPool
import GRPDictionaryGenerator
import TypeCheckProblem

main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  processArgs args

--syntax: --start m n name --iterations i --no-output
--syntax: --load path --iterations 0
--syntax: --testrun
--syntax: --truncate path newname --iterations n
processArgs :: [String] -> IO ()
processArgs ["--init"] = do
  mkDictionary
  generateTestCases 400
processArgs ["--testrun"] = testrun
processArgs ("--start" : gain : min : name : "--iterations" : it : options) =
  initialPool (read gain) (read min) name  >>= runPool (read it) options
processArgs ("--load"  : path              : "--iterations" : it : options) =
  loadFromFile path                        >>= runPool (read it) options
processArgs ("--truncate" : path : newname : "--iterations" : it : options) =
  loadFromFile path >>= truncateP newname  >>= runPool (read it) options
processArgs _ = putStrLn "invalid operands.\nOperands are:\n  --testrun\n  --start max min name --iterations n [--no-output]\n  --load path --iter...\n  --truncate path newname --iter..."


testrun :: IO()
testrun = do
  putStrLn "Starting test run."
  generateTestCases 30
  ip <- initialPool 100 50 "defaultTest"
  runPool 3 [] ip
  putStrLn "Done!"
