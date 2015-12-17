module GRPMath
( weightedAssign
, mean
, variance
, rawHistogram
, discretizeHistogram
)
where

import Debug.Trace
import Data.List
import Data.Ord

--Statistics functions
--TODO: there's probably a gaussian package somewhere on hackage already.

mean :: (Fractional a, Foldable t) => t a -> a
mean lst = sum lst / fromIntegral ( length lst)

variance :: (Fractional a, Foldable t) => t a -> a
variance lst =
  let mn = mean lst
  in foldl (\buff val -> buff + (val - mn) * (val - mn) ) 0 lst / fromIntegral (length lst - 1)

--TODO: diversity metric: ForAll pair of individuals, calculate distance to common ancestor, sum up.

rawHistogram :: Ord t => [t] -> [(Int, t)]
rawHistogram lst = map (\lst -> (length lst, head lst)) $ group $ sort lst

discretizeHistogram
  :: (Enum a1, Eq a, Num a, Num a1, Ord a1) =>
     a1 -> [(a, a1)] -> [(a, (a1, a1))]
discretizeHistogram step hist =
  let
    intervalDelim = [snd $ head hist, (snd (head hist) + step).. (snd (last hist) + step)] --(snd (last hist) + step)]
    intervals = [(0, (x,x+step)) | x <- intervalDelim]
    zipping hist pt =
      case hist of
        [] -> either (\x -> [(0, (x, x))]) (error "discretizeHistogram: illegal state 1") pt
        ((num, (begin, end)):xs) ->
          case pt of
            Left border -> (0, (border, border)):(num, (begin, border)):xs
            Right (num2, val) -> (num2 + num, (begin, end)):xs
  in
    dropWhile (\x -> fst x == 0) $ reverse $ dropWhile (\x -> fst x == 0) $ foldl zipping [] (sortBy (comparing (either id snd)) (map Left intervalDelim ++ map Right hist))

--do p <- loadFromFile "wedevetest-301"; sequence map print $ rawHistogram $ map fitness $ flatten $ genomes p
--do p <- loadFromFile "wedevetest-301"; putStrLn $ show $ rawHistogram  $ map compilationRate $ catMaybes $ iterateTZipper getFeatures $ fromTree $ genomes p
--do p <- loadFromFile "/home/viktor/Dropbox/GA/data/wedtest-241"; putStrLn $ show $ discretizeHistogram 0.01 $ rawHistogram $ map (snd . getFitness) $ filter (\i -> (fst $ getFitness i) == Compilation) $ flatten $ genomes p

--How to assign a number of tickets to a list of weights?
weightedAssign :: (Integral a, Integral b, Functor t, Foldable t) => a -> t Float -> t b
weightedAssign n weights =
  let sw = searchWeight 0.0 10000.0 n weights
  in fmap
    (\wt -> floor (wt * sw) )
    weights

--I'd rather this be double, but that forces double upon fitness.
--Refactoring not worth it currently.
epsilon :: Float
epsilon = 0.0000001
--in case there's no perfect division (most likely in case of exact multiples,
--which is not likely), fewer tokens will be assigned.

--searchWeight :: Double -> Double -> Int -> [Double] -> Double
searchWeight low high n weights =
  if high - low < epsilon
  then trace ("Search weight: " ++ show low) low
  else
    let midCnt = (getTotal ((low+high)/2) weights)
    in if midCnt == n
    then trace ("Search weight: " ++ show ((low+high)/2)) ((low+high)/2)
    else
      if midCnt > n
      then searchWeight low ((low+high)/2) n weights
      else searchWeight ((low+high)/2) high n weights

--getTotal :: (Integral a, RealFrac r) => r -> [r] -> a
getTotal numPerWeight weights = sum $ fmap
  (\wt -> floor $ (wt * numPerWeight) )
  weights
