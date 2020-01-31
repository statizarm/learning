module FSM where

import State

type FSM s = State s s

fsm :: (ev -> s -> s) -> (ev -> FSM s)
fsm transition = \e -> State (\s -> (s, transition e s))

type Speaker = (SpeakerState, Level)

data SpeakerState = Work | Sleep
	deriving (Show)

data Level = Level Int
	deriving (Show)

quiter :: Level -> Level
quiter (Level n) = Level . max 0 $ n - 1

louder :: Level -> Level
louder (Level n) = Level . min 10 $ n + 1

data Usr = Button | Quiter | Louder | Other
	deriving (Show)

speaker :: Usr -> FSM Speaker
speaker = fsm $ trans
	where trans :: Usr -> Speaker -> Speaker
	      trans Button (Sleep, l) = (Work , l)
	      trans Button (Work , l) = (Sleep, l)
	      trans Louder (s    , l) = (s    , louder l)
	      trans Quiter (s    , l) = (s    , quiter l)

