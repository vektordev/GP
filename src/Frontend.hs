import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
--import Graphics.Gloss.Data.Bitmap

import System.Environment (getArgs)

import GRPPool
import GRPIndividual
import GRPFitness

import Data.Maybe

import Data.Tree
import Data.Tree.Zipper

main :: IO()
main = do
  args <-getArgs
  pool <- loadFromFile (head args)
  display (InWindow ("Evaluation of " ++ head args) (1920, 1080) (0,0)) white (makePicture pool)

makePicture :: Pool -> Picture
makePicture p = Pictures [
    summaryPrint p,
    Translate (-10000) 0 $ Scale 10 10 $ plotFeatures $ flatten (extractFromTreeContext getFeatures $ genomes p)--(iterateTZipper getFeatures $ fromTree $ genomes p) (getWeights $ genomes p)
    ]--Polygon [(0,0),(100,100), (300,100), (200,0),(100,200)]-- Text "Hello World"
a =extractFromTreeContext

summaryPrint :: Pool -> Picture
summaryPrint p = Pictures [
    Translate 0 300 $ Text ("IDs handed out = " ++ show (nextID p)),
    Translate 0 150 $ Text ("Individuals found: " ++ show (length $ flatten $ genomes p)),
    Text ("Compiling genomes: "++ show (length $ filter (\ind -> getFitness ind >= (UnknownCompilerError, 10^10)) $ flatten $ genomes p))
  ]

plotFeatures :: [Maybe FeatureVec] -> Picture
plotFeatures fs = Pictures (map plotFeaturesSingle fs ++ axes)
  where
    plotFeaturesSingle (Just fv) = Color (if isLocalMax fv then red else black) $ Translate (1000 * fitness fv) (100 * fromRational (compilationRate fv)) $Circle  (0.3 + activeRegression fv)
    plotFeaturesSingle _ = Blank
    axes = [Line [(0,0),(0,100), (1000,100),(1000,0),(0,0)], Line [(900,0),(900,100)]]

  -- , isLocalMax :: Bool
  -- , generation :: Int
  -- , fitness :: Float
  -- , compilationRate :: Ratio Integer
  -- , avgChildFit :: Float
