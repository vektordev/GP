import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
--import Graphics.Gloss.Data.Bitmap

import System.Environment (getArgs)

import GRPPool
import GRPIndividual
import GRPFitness
import GRPMath

import Data.List
import Data.Maybe

import Data.Tree
import Data.Tree.Zipper

import Debug.Trace

main :: IO()
main = do
  args <-getArgs
  pool <- loadFromFile (head args)
  display (InWindow ("Evaluation of " ++ head args) (1920, 1080) (0,0)) white (makePicture pool)

makePicture :: Pool -> Picture
makePicture p = Pictures [
    Translate (-150) (-110) $ summaryPrint p (catMaybes features),
    Translate (-100) 0 $ plotFeatures "0 - fitness - 1" "0 - compilation rate - 1" fitness (fromRational . compilationRate) features, --(iterateTZipper getFeatures $ fromTree $ genomes p) (getWeights $ genomes p)
    Translate (-210) 0 $ plotFeatures "0 - children - 100" "0 - compilationRate - 1" (\fv -> ((1/100) * (fromIntegral $ GRPPool.children fv))) (fromRational . compilationRate) features,
    Translate 10 0 $ plotFeatures "0 - children - 100" "-1 - cRateGain - 1" (\fv -> ((1/100) * (fromIntegral $ GRPPool.children fv))) (\fv -> (0.5) + 0.5 * fromRational (compilationRateGain fv)) features,
    Translate 120 0 $ plotFeatures "0 - compilationRate - 1" "-1 - cRateGain - 1" (fromRational . compilationRate) (\fv -> (0.5) + 0.5 * fromRational (compilationRateGain fv)) features,
    Translate 230 0 $ plotFeatures "0 - id - 140000" "0 - compilationRate - 1" (\fv -> (fromIntegral $ GRPPool.id fv) / 140000) (fromRational . compilationRate) features,
    Translate 340 0 $ plotFeatures "0 - id - 140000" "0 - fitness - 1" (\fv -> (fromIntegral $ GRPPool.id fv) / 140000) (fitness) features,
    Translate (-210) 110 $ plotHistogram "compRate" "0 - number " (map (fromRational . compilationRate) $ catMaybes features) 0.001,
    Translate (-100) 110 $ plotHistogram "fitness" "0 - number " (map fitness $ catMaybes features) 0.01,
    Translate 10 110 $ plotHistogram "children" "0 - number " (map (fromIntegral . GRPPool.children) $ catMaybes features) 1,
    Translate 120 110 $ plotHistogram "fitgain" "0 - number " (map fitnessGainSinceParent $ catMaybes features) 0.01
    ]
    where
      featureTree = extractFromTreeContext getFeatures $ genomes p
      features :: [Maybe FeatureVec]
      features = flatten featureTree

findCommonAncestor :: ((Maybe FeatureVec) -> Bool) -> Tree (Maybe FeatureVec) -> Tree (Maybe FeatureVec)
findCommonAncestor cond self@(Node fv tFvs) =
  if cond fv
    then self
    else if twoHaveElements tFvs
      then self
      else trace ("step") $findCommonAncestor cond (head $ filter hasElement tFvs)
  where
    hasElement :: Tree (Maybe FeatureVec) -> Bool
    hasElement tree = any cond tree
    twoHaveElements :: [Tree (Maybe FeatureVec)] -> Bool
    twoHaveElements forest = 2 <= (length $ filter hasElement forest)

summaryPrint :: Pool -> [FeatureVec] -> Picture
summaryPrint p fvs = Scale 0.1 0.1 $ Pictures [
    Translate 0 600 $ Text (show $ rawHistogram $ map isLocalMax fvs),
    Translate 0 450 $ Text (show $ rawHistogram $ map state fvs),
    Translate 0 750 $ Text (show $ rawHistogram $ map (fst . getFitness) $ flatten $ genomes p),
    Translate 0 300 $ Text ("IDs handed out = " ++ show (nextID p)),
    Translate 0 150 $ Text ("Individuals found: " ++ show (length $ flatten $ genomes p)),
    Text ("Compiling genomes: "++ show (length $ filter (\ind -> getFitness ind >= (UnknownCompilerError, 10^10)) $ flatten $ genomes p))
  ]

plotFeatures :: String -> String -> (FeatureVec-> Float) -> (FeatureVec-> Float) ->  [Maybe FeatureVec] -> Picture
plotFeatures lb1 lb2 ft1 ft2 fs = Pictures (points ++ axes ++ labels)
  where
    plotFeaturesSingle (Just fv) = Color (if GRPPool.id fv == 0 then green else if isLocalMax fv == LocalMax then red else if isLocalMax fv == Inherited then blue else  black) $ Translate (100 * (min 1 $ max 0 $ ft1 fv)) (100 * (min 1 $ max 0 $ ft2 fv)) $Circle  (0.0 + activeRegression fv)
    plotFeaturesSingle _ = Blank
    points = filter (/= Blank) $ map plotFeaturesSingle fs
    axes = [Line [(0,0),(0,100), (100,100),(100,0),(0,0)]] ++ if "cRateGain" `isInfixOf` lb2 then [Color (makeColor 0.5 0.5 0.5 0.5) $ Line [(0,50),(100,50)]] else []
    labels = map (Scale 0.05 0.05) [Translate 0 (-110) $ Text lb1 , Rotate (-90) $ Translate 0 10 $ Text lb2]

plotHistogram lb1 lb2 dat step = Pictures (labels ++ frames ++ blocks)
  where
    raw = rawHistogram dat
    hist = discretizeHistogram step raw
    maxStep = snd $ snd $ Prelude.last hist
    minStep = fst $ snd $ head hist
    maxCount = fromIntegral $ maximum $ map fst hist
    getNormalized pos = (pos - minStep) / (maxStep - minStep)
    blocks = map (\(n, (from, to)) -> Scale 100 100 $ Polygon [(getNormalized from, 0),(getNormalized to, 0),(getNormalized to, ((fromIntegral n) / maxCount)),(getNormalized from, ((fromIntegral n) / maxCount)),(getNormalized from, 0)]) hist
    frames = [Line [(0,0), (0,100),(100,100),(100,0), (0,0)]]
    labels = map (Scale 0.05 0.05) [Translate 0 (-110) $ Text (show minStep ++ lb1 ++ show maxStep) , Rotate (-90) $ Translate 0 10 $ Text (lb2 ++ show maxCount)]
  -- , isLocalMax :: Bool
  -- , generation :: Int
  -- , fitness :: Float
  -- , compilationRate :: Ratio Integer
  -- , avgChildFit :: Float
