module GRPSafety
( isSafe
, getSafeLines
, getSafetyPrefix
, dropSafetyPrefix
) where

import Data.List (isInfixOf)
import Data.Maybe
import Debug.Trace

--This Module deals with the safety prefix of genomes
--  (The source code up to "safeLines = n", designed to prevent careless change to the Genome's boilerplate)
--as well as various other safety criteria of the Genome
--NB: The safety prefix is not considered part of the Genome and can not be seen nor changed by the Code Gen.
--All these criteria should ensure that malicious code in a -XSafe-compiled genome can not have any ill effect.

getSafeLines :: String -> Maybe Int
getSafeLines source =
  let
    safeLinesInSource = filter (\line -> take 12 line == "safeLines = ") $ lines source
    safeLinesCount = read $ drop 12 $ head safeLinesInSource
    safeLinesFromValue = [lines source !! (safeLinesCount - 1)]
  in if safeLinesInSource /= safeLinesFromValue then Nothing else Just safeLinesCount

getSafetyPrefix :: String -> Maybe String
getSafetyPrefix source = fmap (\sl -> unlines $ take sl $ lines source) $ getSafeLines source

dropSafetyPrefix :: String -> Maybe String
dropSafetyPrefix source = fmap (\sl -> unlines $ drop sl $ lines source) $ getSafeLines source

--list of words that should not ever be in a program we evaluate.
prohibited = ["{-#", "#-}", "import", "IO"]

isSafe :: String -> (Bool, String)
isSafe source = do
  let
    saneSafeLines = isJust $ getSafeLines source
    containsBadWords =  any (\badWord -> badWord `isInfixOf` (fromJust $ dropSafetyPrefix source)) prohibited --drop the safety prefix here
    hasBadChars = not $ null $ filter (\char -> not $ elem char ('\r':'\t':'\n':[' '..'~'])) source
  if not saneSafeLines
  then trace "GRPSafety: Error with safeLines" (False, "Safety: Please check safeLines limit of source code")
  else if containsBadWords
  then (False, "Safety: Prohibited words detected.")
  else if hasBadChars
  then (False, "Safety: Control Characters detected in source code.")
  else (True, "")

testSources = ["{-#\n pragma stuff #-}\nimport stuff\nsafeLines = 4\nhere be pure code", "safeLines = 1\nimport", "safeLines = 1\n#-}"]
testResults = [True, False, False]

test :: [Bool]
test = zipWith (\src res -> (fst $ isSafe src) == res) testSources testResults
