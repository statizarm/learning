module Diff where

converge :: (Ord a, Num a) => a -> [a] -> a
converge eps (a : b : as) 
        | abs (a - b) <= eps = a
        | otherwise          = converge eps (b : as)

easyDiff :: (Fractional a) => (a -> a) -> a -> a -> a
easyDiff f x h = (f (x + h) - f x ) / h

diff :: (Ord a, Num a, Fractional a) => (a -> a) -> a -> a
diff f x = converge 1e-5 $ map (easyDiff f x) . iterate ( / 2) $ 1
