module Ps where

data Ps a = a :+: Ps a

instance (Show a, Num a, Eq a) => Show (Ps a) where
	show = (++ "...") . foldr ((++) . (++ " + ")) "" .
	       map (\(d, k) -> show k ++ " x^" ++ show d) .
	       take 5 . filter (\(_, k) -> k /= 0) . 
	       zip [0 .. ] . toList

toList :: Ps a -> [a]
toList ~(a :+: as) = a : toList as

p0 :: Num a => a -> Ps a
p0 a = a :+: p0 0

ps :: Num a => [a] -> Ps a
ps []       = p0 0
ps (a : as) = a :+: ps as

instance Num a => Num (Ps a) where
	abs = undefined
	negate = undefined
	signum = undefined

	(a :+: as) + (b :+: bs) = (b + a) :+: (as + bs)
	(*) = undefined

	fromInteger = p0 . fromInteger
