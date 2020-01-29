{- LANGUAGE MultiWayIf #-} -- Для кошерного if

module Main where

toDigit :: Char -> Int
toDigit '0' = 0
toDigit '1' = 1
toDigit '2' = 2
toDigit '3' = 3
toDigit '4' = 4
toDigit '5' = 5
toDigit '6' = 6
toDigit '7' = 7
toDigit '8' = 8
toDigit '9' = 9
toDigit _   = -1

calc :: String -> Int
calc expr = head . buildExpr expr $ []

push :: Int -> [Int] -> [Int]
push val stack = val : stack

buildExpr :: String -> [Int] -> [Int]
buildExpr ('*' : expr) (scnd : fst : tail) = buildExpr expr . push (fst * scnd)    $ tail
buildExpr ('-' : expr) (scnd : fst : tail) = buildExpr expr . push (fst - scnd)    $ tail
buildExpr ('-' : expr) (fst : [])          = buildExpr expr . push (-fst)          $ []
buildExpr ('+' : expr) (scnd : fst : tail) = buildExpr expr . push (fst + scnd)    $ tail
buildExpr (digit : expr) argStack          = buildExpr expr . push (toDigit digit) $ argStack
buildExpr [] argStack                      = argStack

main :: IO ()
main = print (calc "2-23*+3-")
