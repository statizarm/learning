module Test where

import Prelude

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs@(x:xs') = let
    left = filter (< x) xs
    right = filter (> x) xs
    same = filter (== x) xs
  in qsort left ++ same ++ qsort right
