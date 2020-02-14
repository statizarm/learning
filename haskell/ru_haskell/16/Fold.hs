module Fold where

import Fix

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f = f . fmap (fold f) . unFix

unfold :: Functor f => (a -> f a) -> a -> Fix f
unfold f = Fix . fmap (unfold f) . f
