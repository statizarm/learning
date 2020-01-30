module Main where

import System.Environment
import Control.Applicative

main :: IO ()
main = putStrLn =<< reply <$> getProgName <*> getArgs

reply :: String -> [String] -> String
reply name (x : _) = name ++ ": " ++ case x of
	"happy"   -> "What a lovely day. What's up?"
	"Sad"     -> "Oohh. Have you got some news for me?"
	"neutral" -> "How are you?"
reply name _       = reply name ["neutral"]
