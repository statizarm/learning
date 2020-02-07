module Unfold where

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f = \b -> case f b of
	Just (a, b') -> a : unfoldr f b'
	Nothing      -> []

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f = maybe [] (\(a, b) -> a : unfoldr f b) . f
