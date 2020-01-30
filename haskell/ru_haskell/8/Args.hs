module Main where

import System.Environment

main :: IO ()
main = getArgs >>= mapM_ putStrLn . zipWith f [1 .. ]
	where f x y = show x ++ ": " ++ y
