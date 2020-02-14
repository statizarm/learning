module List where

import Fix

data L a b =  Nil | Cons a b
	deriving (Show, Eq)

type List a = Fix (L a)

nil :: List a
nil = Fix Nil

cons :: a -> List a -> List a
cons a = Fix . Cons a

instance Functor (L a) where
    fmap f x = case x of
        Nil -> Nil
	Cons a b -> cons a $ f b
