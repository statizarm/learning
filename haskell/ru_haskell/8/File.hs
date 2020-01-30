module Main where

main :: IO ()
main = helloMsg >> getLine >>= read >>= append
	where read file   = readFile file >>= putStrLn >> return file
	      append file = appMsg >> getLine >>= appendFile file
	      helloMsg    = putStr "Hello, please enter filepath: \n"
	      appMsg      = putStr "Please enter appended message: \n"

