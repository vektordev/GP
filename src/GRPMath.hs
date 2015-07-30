module GRPMath
( weightedAssign
, mean
, variance
)
where

import Debug.Trace

--Statistics functions
mean lst = (sum lst) / (fromIntegral $ length lst)

variance lst =
  let mn = mean lst
  in (sum $ map (\elem -> (elem-mn)^2) lst ) / (fromIntegral $ (length lst - 1 ))

--How to assign a number of tickets to a list of weights?
weightedAssign :: (RealFrac a, Integral b) => b -> [a] -> [b]
weightedAssign n weights = map (\wt -> floor $ (wt * (searchWeight 0.0 10000.0 n weights) ) ) weights

epsilon = 0.00001
--in case there's no perfect division (most likely in case of exact multiples, which is not likely), fewer tokens will be assigned.

searchWeight :: (RealFrac a, Integral b) => a -> a -> b -> [a] -> a
searchWeight low high n weights =
  if high - low < epsilon
  then low
  else
    let midCnt = (getTotal ((low+high)/2) weights) :: Int
    in if midCnt == n
    then ((low+high)/2)
    else
      if midCnt > n
      then trace (show low ++ show high) $ searchWeight low ((low+high)/2) n weights
      else trace (show low ++ show high) $ searchWeight ((low+high)/2) high n weights

getTotal :: (Integral a, RealFrac r) => r -> [r] -> a
getTotal numPerWeight weights = sum $ map (\wt -> floor $ (wt * numPerWeight) ) weights
