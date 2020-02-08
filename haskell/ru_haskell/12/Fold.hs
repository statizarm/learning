module Fold where

data Tree a = Node a [Tree a]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = \x -> let Node a as = x
                   in  f a . map (foldTree f) $ as

depth :: Tree a -> Int
depth = foldTree $ \_ bs -> 1 + foldr max 0 bs

sumTree :: (Num a) => Tree a -> a
sumTree = foldTree $ \a as -> a + sum as

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

prod' :: Num a => [a] -> a
prod' = foldr (*) 1

cycle' :: [a] -> [a]
cycle' as = foldr (:) (cycle' as) as

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True

length' :: [a] -> Int
length' = foldr (\_ x-> x + 1) 0

unzip' :: [(a,b)] -> ([a], [b])
unzip' = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], [])


