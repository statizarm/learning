module Test where

import Data.Char
import Data.Maybe

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = maybe 100 id $ fmap (+) (f y) <*> fmap (* 10) (f x)
    where f = \x -> if isDigit(x) then Just $ digitToInt x else Nothing

