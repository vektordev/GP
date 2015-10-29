module GRPMath
( weightedAssign
, mean
, variance
)
where

import Debug.Trace

--Statistics functions
--TODO: there's probably a gaussian package somewhere on hackage already.

mean :: (Fractional a, Foldable t) => t a -> a
mean lst = sum lst / fromIntegral ( length lst)

variance :: (Fractional a, Foldable t) => t a -> a
variance lst =
  let mn = mean lst
  in foldl (\buff val -> buff + (val - mn) * (val - mn) ) 0 lst / fromIntegral (length lst - 1)

--diversity metric: ForAll pair of individuals, calculate distance to common ancestor, sum up.

--How to assign a number of tickets to a list of weights?
--TODO: This needs to be incorporated into GRPCore.
--weightedAssign :: Int -> [Double] -> [Int]
weightedAssign n weights = fmap
  (\wt -> floor (wt * searchWeight 0.0 10000.0 n weights) )
  weights

--I'd rather this be double, but that forces double upon fitness.
--Refactoring not worth it currently.
epsilon :: Float
epsilon = 0.00001
--in case there's no perfect division (most likely in case of exact multiples,
--which is not likely), fewer tokens will be assigned.

--searchWeight :: Double -> Double -> Int -> [Double] -> Double
searchWeight low high n weights =
  if high - low < epsilon
  then low
  else
    let midCnt = (getTotal ((low+high)/2) weights)
    in if midCnt == n
    then ((low+high)/2)
    else
      if midCnt > n
      then trace (show low  ++ ";" ++ show high) $ searchWeight low ((low+high)/2) n weights
      else trace (show low ++ show high) $ searchWeight ((low+high)/2) high n weights

--getTotal :: (Integral a, RealFrac r) => r -> [r] -> a
getTotal numPerWeight weights = sum $ fmap
  (\wt -> floor $ (wt * numPerWeight) )
  weights
