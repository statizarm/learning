module Kleisli where

class Kleisli m where
	idK :: a -> m a
	(*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
