module Game where

import Prelude hiding (Either (..))
import Data.Array
import System.Random

data Move = Up | Down | Left | Right

type Pos = (Int, Int)

type Label = Int

type Board = Array Pos Label

data Game = Game {
    emptyField :: Pos,
    gameBoard  :: Board}
    deriving (Eq)

instance Show Game where
    show (Game _ board) = "\n" ++ space ++ line ++
        (foldr (\a b -> a ++ space ++ line ++ b) "\n" $
	 map column [0 .. 3])
	where post id     = showLabel $ board ! id
	      showLabel n = cell $ show $ case n of
	                                  15 -> 0
					  n  -> n + 1
	      cell "0"    = "    "
	      cell [x]    = ' ' : ' ' : x : ' ' : []
	      cell [a,b]  = ' ' : a   : b : ' ' : []
	      line        = "+----+----+----+----+\n"

	      nums = ((space ++ "|") ++ ) . foldr
	             (\a b -> a ++ "|" ++ b) "\n" . map post

	      column i = nums $ map (\x -> (i, x)) [0 .. 3]
	      space    = "\t"

isGameOver :: Game -> Bool
isGameOver = ( == initGame)

initGame :: Game
initGame = Game (3, 3) $ listArray ((0,0), (3, 3)) [0 .. 15]

move :: Move -> Game -> Game
move m (Game id board)
    | within id' = Game id' $ board // updates
    | otherwise  = Game id board
    where id' = shift (orient m) id
          updates = [(id', emptyLabel), (id, board ! id')]

within :: Pos -> Bool
within (x, y) = p x && p y
    where p a = a <= 3 && a >= 0

newtype Vec = Vec (Int, Int)

orient :: Move -> Vec
orient m = Vec $ case m of
    Left  -> (0 , -1)
    Right -> (0 ,  1)
    Up    -> (-1,  0)
    Down  -> (1 ,  0)

shift :: Vec -> Pos -> Pos
shift (Vec (va, vb)) (a, b) = (a + va, b + vb)

emptyLabel :: Label
emptyLabel = 15

shuffle :: Int -> IO Game
shuffle n = (iterate (shuffle1 =<<) $ pure initGame) !! n

shuffle1 :: Game -> IO Game
shuffle1 game = flip move game <$> (randomElem $
                nextMoves game)

randomElem :: [a] -> IO a
randomElem as = (as !!) <$> randomRIO (0, length as - 1)

nextMoves :: Game -> [Move]
nextMoves game = filter (within . moveEmptyTo . orient) $ 
              allMoves
    where moveEmptyTo v = shift v (emptyField game)
          allMoves      = [Left, Right, Up, Down]
