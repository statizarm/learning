module Test where

integration f a b = let
    delta = (b - a) / nRanges
    nRanges = 100000
  in (/ 2) $ g f a delta nRanges
  where
    g f a d 0 = 0
    g f a d n = let
        area = (f a + f next) * d
        next = a + d
      in area + g f next d (n - 1)
