module Test where

fib' :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer -> Integer
fib' f x y 0 = y
fib' f x y n = fib' f (f y x) x (n - 1)

posFib :: Integer -> Integer
posFib = fib' (+) 1 0

negFib :: Integer -> Integer
negFib = fib' (-) 1 0

fib :: Integer -> Integer
fib n = if n < 0 then negFib $ -n else posFib n
