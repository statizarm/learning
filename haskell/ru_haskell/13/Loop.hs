module Loop where

import Prelude hiding (Either (..))
import Game
import Data.Char

data Query = Quit | NewGame Int | Play Move

play :: IO ()
play = greetings >> setup >>= gameLoop

greetings :: IO ()
greetings = helloMsg >> rules

helloMsg :: IO ()
helloMsg = putStrLn $
    "Привет, это моя первая игра, так что не суди строго\n"

rules :: IO ()
rules = undefined

setup :: IO Game
setup = putStrLn "Хочешь сыграть?" >>
        putStrLn "Введи уровень сложности (число): " >>
        getLine >>= maybe setup shuffle . readInt

readInt :: String -> Maybe Int
readInt x = if all isDigit x
            then Just $ read x
	    else Nothing

shuffle :: Int -> IO Game
shuffle = undefined
 
gameLoop :: Game -> IO ()
gameLoop game
    | isGameOver game = showResults game >> setup >>=
                        gameLoop
    | otherwise       = print game >> askForQuery >>=
                        reactOnQuery game

showResults :: Game -> IO ()
showResults game = print game >> putStrLn "Game over!"

askForQuery :: IO Query
askForQuery = getLine >>= maybe askAgain return . parseQuery 
    where askAgain = wrongQuery >> askForQuery

parseQuery :: String -> Maybe Query
parseQuery x = case x of
    "up"          -> Just $ Play Up
    "down"        -> Just $ Play Down
    "left"        -> Just $ Play Left
    "right"       -> Just $ Play Right
    "q"           -> Just $ Quit
    'n' : 'g' : n -> Just . NewGame =<< readInt n

    otherwise     -> Nothing

wrongQuery :: IO ()
wrongQuery = putStrLn "Wrong query, please try again"

reactOnQuery :: Game -> Query -> IO ()
reactOnQuery game query = case query of
    Quit      -> quit
    NewGame n -> gameLoop =<< shuffle n
    Play m    -> gameLoop $ move m game

quit :: IO ()
quit = putStrLn "Пока!" >> return ()
