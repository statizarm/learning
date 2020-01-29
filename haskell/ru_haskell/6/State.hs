module State where

import Kleisli
import Control.Monad

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Kleisli (State s) where
	idK       = \a -> State (\s -> (a, s))
	(*>) f g  = \a -> State (\s -> let State f' = f a
	                                   (b, s')  = f' s
			                   State g' = g b
			               in g' s')

instance Functor (State s) where
        fmap f (State g) = State (\s -> let (a, s') = g s
	                                in  (f a, s'))

instance Applicative (State s) where
        pure      = \a -> State (\s -> (a, s))
	sf <*> sx  = State (\s -> let (f, s')  = runState sf s
                                      (a, s'') = runState sx s'
				  in  (f a, s''))

instance Monad (State s) where
	return = \a -> State (\s -> (a, s))
	m >>= ma = State $ \s0 ->
			let (b, s1) = runState m s0
			in runState (ma b) s1
