module GhciEval (
  eval
) where

import System.Process (readProcessWithExitCode)
import Control.Monad
import Data.List
import Language.Haskell.Interpreter (runInterpreter, loadModules, setTopLevelModules, setImportsQ, typeOf, Interpreter(..), liftIO)

dropUpTo :: String -> String -> String
dropUpTo "" lst = lst
dropUpTo _ "" = ""
dropUpTo substr str = if substr `isPrefixOf` str
  then drop (length substr) str
  else dropUpTo substr (tail str)
{-
eval :: String -> IO String
eval prog = do
  (code, out, err) <- readProcessWithExitCode "ghci" ["GRPSeed.hs"] (":t " ++ prog)
  --Omitted overly verbose debug message:
  --unless (null err) $ putStrLn ("There were errors when eval'ing an expression for typecheck problem:\n" ++ err)
  let filteredLns = filter (\str -> head str == '*') $lines out
  let typedecl =  dropUpTo ":: " $ head filteredLns
  return typedecl
-}

eval :: String -> IO String
eval prog = do
  r <- runInterpreter $ testHint prog
  case r of
    Left err -> do
      putStrLn ("error in eval: " ++ show err)
      return ""
    Right str -> return str

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""

--TODO: Profile this mess:

--import Criterion.Measurement
--getTime

-- observe that Interpreter () is an alias for InterpreterT IO ()
testHint :: String -> Interpreter String
testHint prog =
  do
    loadModules ["GRPSeed.hs"]
    setTopLevelModules ["GRPSeed"]
    setImportsQ [("Prelude", Nothing)]
    pType <- typeOf prog
    pType' <- typeOf ("(" ++ prog ++ ")")
    say pType'
    return pType
