{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Nat where

import Prelude hiding (succ)
import Fix
import Fold

data N a = Zero | Succ a
	deriving (Show, Eq)

type Nat = Fix N

zero :: Nat
zero = Fix Zero

succ :: Nat -> Nat
succ = Fix . Succ

instance Functor N where
    fmap _ Zero     = Zero
    fmap f (Succ a) = Succ (f a)

instance Num Nat where
	(+) a = fold $ \x -> case x of
            Zero   -> a
	    Succ x -> succ x
	
	(*) a = fold $ \x -> case x of
	    Zero   -> zero
	    Succ x -> a + x
	
	fromInteger = unfold $ \n -> case n of
	    0 -> Zero
	    n -> Succ (n - 1)
	
	abs    = undefined
	signum = undefined
	negate = undefined
