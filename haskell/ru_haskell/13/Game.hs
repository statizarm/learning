module Game where

import Prelude hiding (Either (..))

data Move = Up | Down | Left | Right

type Pos = (Int, Int)

type Label = Int

data Board = Array Pos Label

data Game = Game {
    emptyField :: Pos,
    gameBoard  :: Board}

instance Show Game where
    show = undefined

move :: Move -> Game -> Game
move = undefined

isGameOver :: Game -> Bool
isGameOver = undefined
