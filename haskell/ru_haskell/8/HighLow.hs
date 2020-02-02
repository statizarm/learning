module Main where

import System.Environment
import System.Random

import Control.Monad

range :: [String] -> (Int, Int)
range (a : b : _) = (read a, read b)
range _           = error "check number of arguments"

checkNum :: Int -> (Int -> IO ()) -> Int -> IO ()
checkNum a f b | a == b = putStrLn "Correct!!!"
               | a < b  = putStrLn "Your number is higher" >> f a
	       | a > b  = putStrLn "Your number is lower" >> f a

main :: IO ()
main = helloMsg >> fmap range getArgs >>= randomRIO >>= cycle
	where helloMsg = putStrLn "Hi, let's play in Lower Higher game!"
	      cycle x  = queMsg >> getLine >>= readIO >>= checkNum x cycle
	      queMsg   = putStr "enter your number:\n"
