module Main where

import System.Environment
import System.Random

import Control.Monad
import Control.Applicative

game :: Int -> IO ()
game win = do
	xs <- fmap (randomRs (1, 6)) newStdGen
	ys <- fmap (randomRs (1, 6)) newStdGen
	cycl 0 0 win $ zipWith (+) xs ys

cycl :: Int -> Int -> Int -> [Int] -> IO ()
cycl x y win (rx : ry : rs) | x == y && x == win = drawMsg
                            | x == win      = winFstMsg
			    | y == win      = winSndMsg
			    | x > win       = cycl 0 y win (rx : ry : rs)
                            | y > win       = cycl x 0 win (rx : ry : rs)
			    | otherwise     = showFst >> showSnd >> getLine >>
			                      cycl (x + rx) (y + ry) win rs
	where drawMsg   = putStrLn "draw"
	      winFstMsg = putStrLn "First player wins!"
	      winSndMsg = putStrLn "Second player wins!"
	      showFst   = putStrLn $ "First player points: " ++ show x
	      showSnd   = putStrLn $ "Second player points: " ++ show y

main :: IO ()
main = helloMsg >> getArgs >>= winPoints >>= game 
	where winPoints (x : _) = readIO x
	      winPoints _       = error "Check number of arguments"
	      helloMsg          = putStrLn "hi, let's play in dice"
