module Test where

data Odd = Odd Integer
  deriving(Eq, Show)

instance Enum Odd where
  pred (Odd x) = Odd $ x - 2
  succ (Odd x) = Odd $ x + 2

  toEnum x = Odd . toInteger $ x * 2 + 1
  fromEnum (Odd x) = fromInteger $ div (x - 1) 2

  enumFrom x = x : enumFrom (succ x)
  enumFromTo x y = takeWhile (/= y) (enumFrom x) ++ [y]
  enumFromThenTo x y z = takeWhile (/= z) (enumFromThen x y) ++ [z]
  enumFromThen (Odd x) (Odd y) = iterate (\(Odd i) -> Odd $ i + y - x) (Odd x)

