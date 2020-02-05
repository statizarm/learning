module Stream where

import Data.List

dt :: Fractional a => a
dt = 1e-3

int :: Fractional a => a -> [a] -> [a]
int x0 ~(f : fs) = x0 : int (dt * f + x0) fs

time :: (Enum a, Fractional a) => [a]
time = [0, dt .. ]

dist :: (Fractional a) => Int -> [a] -> [a] -> a
dist n a b = (/ fromIntegral n) $ foldl' (+) 0 $ 
             take n $ map abs $ zipWith (-) a b
