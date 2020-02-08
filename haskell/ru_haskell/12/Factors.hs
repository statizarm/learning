module Factors where

import TreeUnfold


fact :: (Integral a) => a -> [a]
fact a = find a [2, 3 .. a - 1]
	where find x (a : as) = let k = x `div` a
	                        in  if x `mod` k == 0
				    then a : k : []
				    else find x as
	      find _ []       = []
