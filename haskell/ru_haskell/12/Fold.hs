module Fold where

data Tree a = Node a [Tree a]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = \x -> let Node a as = x
                   in  f a . map (foldTree f) $ as

depth :: Tree a -> Int
depth = foldTree $ \_ bs -> 1 + foldr max 0 bs

sumTree :: (Num a) => Tree a -> a
sumTree = foldTree $ \a as -> a + sum as
