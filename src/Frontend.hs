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
    Translate (-1000) 0 $ Scale 10 10 $ plotFeatures "0 - fitness - 1" "0 - compilation rate - 1" fitness (fromRational . compilationRate) features, --(iterateTZipper getFeatures $ fromTree $ genomes p) (getWeights $ genomes p)
    Translate (-2100) 0 $ Scale 10 10 $ plotFeatures "0 - children - 100" "0 - compilationRate - 1" (\fv -> ((1/100) * (fromIntegral $ GRPPool.children fv))) (fromRational . compilationRate) features
    ]
    where
      features = flatten (extractFromTreeContext getFeatures $ genomes p)

summaryPrint :: Pool -> Picture
summaryPrint p = Pictures [
    Translate 0 300 $ Text ("IDs handed out = " ++ show (nextID p)),
    Translate 0 150 $ Text ("Individuals found: " ++ show (length $ flatten $ genomes p)),
    Text ("Compiling genomes: "++ show (length $ filter (\ind -> getFitness ind >= (UnknownCompilerError, 10^10)) $ flatten $ genomes p))
  ]

plotFeatures :: String -> String -> (FeatureVec-> Float) -> (FeatureVec-> Float) ->  [Maybe FeatureVec] -> Picture
plotFeatures lb1 lb2 ft1 ft2 fs = Pictures (map plotFeaturesSingle fs ++ axes ++ labels)
  where
    plotFeaturesSingle (Just fv) = Color (if isLocalMax fv then red else black) $ Translate (100 * (min 1 $ max 0 $ ft1 fv)) (100 * (min 1 $ max 0 $ ft2 fv)) $Circle  (0.0 + activeRegression fv)
    plotFeaturesSingle _ = Blank
    axes = [Line [(0,0),(0,100), (100,100),(100,0),(0,0)]]-- , Line [(900,0),(900,100)]]
    labels = map (Scale 0.05 0.05) [Translate 0 (-110) $ Text lb1 , Rotate (-90) $ Translate 0 10 $ Text lb2]

  -- , isLocalMax :: Bool
  -- , generation :: Int
  -- , fitness :: Float
  -- , compilationRate :: Ratio Integer
  -- , avgChildFit :: Float
