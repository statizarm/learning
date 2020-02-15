module List where

import Fix
import Fold
import Nat

data L a b =  Nil | Cons a b
	deriving (Show, Eq)

type List a = Fix (L a)

nil :: List a
nil = Fix Nil

cons :: a -> List a -> List a
cons a = Fix . Cons a

instance Functor (L a) where
    fmap f x = case x of
        Nil      -> Nil
	Cons a b -> Cons a $ f b

headL :: List a -> a
headL as = case unFix as of
    Nil      -> error "empty List"
    Cons a _ -> a

tailL :: List a -> List a
tailL as = case unFix as of
    Nil      -> error "empty List"
    Cons _ b -> b

mapL :: (a -> b) -> List a -> List b
mapL f = fold $ \x -> case x of
    Nil      -> nil
    Cons a b -> f a `cons` b

takeL :: Int -> List a -> List a
takeL = curry $ unfold $ \(n, xs) ->
    if n == 0 then Nil
              else Cons (headL xs) (n - 1, tailL xs)

iterateL :: (a -> a) -> a -> List a
iterateL f = unfold $ \a -> Cons a $ f a

appendL :: List a -> List a -> List a
appendL a b = fold (\x -> case x of
    Nil        -> b
    Cons a' b' -> a' `cons` b') a
