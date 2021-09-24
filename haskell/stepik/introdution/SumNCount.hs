module Test where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 = f (-x)
              | otherwise = f x
  where
    f x = if x < 10 then (x, 1) else let
        (s, c) = f $ div x 10
      in (s + mod x 10, c + 1)

