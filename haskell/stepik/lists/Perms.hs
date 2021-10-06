module Test where

import Data.List

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (f x) $ perms xs
  where
    f x' [] = [[x']]
    f x' xs@(x:xs') = [x':xs] ++ map (x:) (f x' xs')
