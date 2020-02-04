{-# LANGUAGE BangPatterns #-}
module Strict where

import Data.List

sum' :: Num a => [a] -> a
sum' = iter 0
	where iter res [] = res
	      iter res (a : as) = let res' = res + a
	                          in  res' `seq` iter res' as

sum'' = iter 0
	where iter res [] = res
	      iter res (a : as) = let res' = res + a
	                          in iter res' as

mean' :: [Double] -> Double
mean' = divided . iter (0, 0)
	where divided (s, l)       = s / l
	      iter res    []       = res
	      iter (s, l) (a : as) = let s' = s + a
	                                 l' = l + 1
			             in  s' `seq` l' `seq` iter (s', l') as

data Res = Res !Double !Double

mean'' :: [Double] -> Double
mean'' = division . foldl' iter (Res 0 0)
	where iter (Res sum len) a   = Res (sum + a) (len + 1)
	      division (Res sum len) = sum / len



