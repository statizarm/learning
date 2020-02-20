module Loop where

import Prelude hiding (Either (..))
import Game
import Data.Char

data Query = Quit | NewGame Int | Play Move | Help

play :: IO ()
play = greetings >> setup >>= gameLoop

greetings :: IO ()
greetings = helloMsg >> rules

helloMsg :: IO ()
helloMsg = putStrLn $
    "Привет, это моя первая игра, так что не суди строго\n"

rules :: IO ()
rules = print initGame >> remindMoves

setup :: IO Game
setup = putStrLn "Хочешь сыграть?" >>
        putStrLn "Введи уровень сложности (число): " >>
        getLine >>= maybe setup shuffle . readInt

readInt :: String -> Maybe Int
readInt x = if all isDigit x
            then Just $ read x
	    else Nothing

gameLoop :: Game -> IO ()
gameLoop game
    | isGameOver game = showResults game >> setup >>=
                        gameLoop
    | otherwise       = print game >> askForQuery >>=
                        reactOnQuery game

showResults :: Game -> IO ()
showResults game = print game >> putStrLn "Game over!"

askForQuery :: IO Query
askForQuery = showAsk >> getLine >>=
              maybe askAgain return . parseQuery 
    where askAgain = wrongQuery >> askForQuery

showAsk :: IO ()
showAsk = putStrLn "Твой ход:"
parseQuery :: String -> Maybe Query
parseQuery x = case x of
    "k"           -> Just $ Play Up
    "j"           -> Just $ Play Down
    "h"           -> Just $ Play Left
    "l"           -> Just $ Play Right
    "q"           -> Just $ Quit
    'n' : 'g' : n -> Just . NewGame =<< readInt n
    "help"        -> Just $ Help

    otherwise     -> Nothing

wrongQuery :: IO ()
wrongQuery = putStrLn "Неверный запрос, попробуй заново, \
                      \или введи help для помощи"

reactOnQuery :: Game -> Query -> IO ()
reactOnQuery game query = case query of
    Quit      -> quit
    NewGame n -> gameLoop =<< shuffle n
    Play m    -> gameLoop $ move m game
    Help      -> remindMoves >> gameLoop game

remindMoves :: IO ()
remindMoves = mapM_ putStrLn moves
    where moves = ["Возможные ходы пустой клетки:",
                   " left  -- Налево",
		   " right -- Направо",
		   " up    -- Вверх",
		   " down  -- Вниз",
		   "Другие действия:",
		   " q          -- Выйти",
		   " ng `число` -- Новая игра с уровнем \
		   \ сложности `число`",
		   "help        -- Вывести это сообщение"]

quit :: IO ()
quit = putStrLn "Пока!" >> return ()
