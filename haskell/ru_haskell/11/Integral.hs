module Integ where

converge :: (Num a, Ord a) => a -> [a] -> a
converge eps (a : b : as)
	| abs (a - b) <= eps = a
	| otherwise          = converge eps (b : as)

integ :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a
integ f a b = converge 1e-5 $ int f a b (f a) (f b)
	where int f a b fa fb = let mid = (a + b) / 2
	                            fm  = f mid
				in  (fa + fb) * (b - a) / 2 :
				zipWith (+) (int f a mid fa fm)
				            (int f mid b fm fb)
	      mid       = (a + b) / 2

