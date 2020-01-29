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

type UsrPipe = [Usr]

mkPipe :: IO UsrPipe
mkPipe = map matchToken <$> words <$> getContents

matchToken :: String -> Usr
matchToken tok | tok == "Button" = Button
               | tok == "Quiter" = Quiter
	       | tok == "Louder" = Louder
	       | otherwise       = Other

main :: IO ()
main = let pipe = mkPipe
           state = mapM speaker <$> pipe
       in  print =<< fmap (($ (Sleep, Level 0)) . runState) state
       
