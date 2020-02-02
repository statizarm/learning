module Main where

import System.Environment

import Control.Applicative

import Data.List

grep :: [String] -> [String] -> String
grep opts str = foldr (++) "" . map (++ "\n") . filter (isIn opts) $ str
	where isIn []       _   = False
	      isIn (w : ws) str = if isInfixOf w str
	                          then True
				  else isIn ws str

main :: IO ()
main = do
	args <- getArgs
	if length args < 2
	then error "Check number of args"
	else do
	     	let (file : opts : _) = args
	     	grep (words opts) <$> fmap lines (readFile file) >>= putStr
