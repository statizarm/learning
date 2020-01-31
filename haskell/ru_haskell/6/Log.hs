module Log where

import Prelude hiding (True, False)

data Log = True
         | False
	 | Not Log
	 | And Log Log
	 | Or  Log Log
	 deriving (Show)

instance Num Log where
	negate a = Not a
	(+) = Or
	(*) = And

	fromInteger 0 = False
	fromInteger _ = True

	abs = undefined
	signum = undefined
