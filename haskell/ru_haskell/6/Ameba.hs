module Ameba where

import FSM

type Ameba = (Action, Light)

type Light = (Direction, Value)

type Direction = (Double, Double)

type Value = Double

data Action = Stay
            | Move
	    deriving (Show)

updateLight :: Light -> Light -> Light
updateLight (da, va) (db, vb) = (go (scale da va) (scale db vb)
                                , va + vb)
        where go (ax, ay) (bx, by) = norm (ax + bx, ay + by)
	      scale (x, y) k       = (x * k, y * k)
	      norm (0, 0)          = (0, 0)
	      norm (x, y)          = let len = sqrt (x^2 + y^2)
	                             in (x / len, y / len)

update :: Light -> FSM Ameba
update l = fsm trans l
        where trans stepLight (act, light) =
	       case updateLight light stepLight of
	       (_, 0)          -> (Move, ((1, 1), 0))
	       l @ ((0, 0), v) -> (Stay, l)
	       l               -> (Move, l)
