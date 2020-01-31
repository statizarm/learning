module CNF where

import Log

data Or' a  = Or' [a]
data Not' a = Not' a
data And' a = And' [a]
data Lit    = False'
            | True'
