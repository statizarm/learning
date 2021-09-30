module Test (
    f1, -- явный экспорт функции f1
    f2 -- явный экспорт функции f2
) where

import Data.Char hiding (toLower) -- импортирование модуля за исключением функции toLower
import qualified Data.Set as Set -- импортирование модуля с необходимостью вызова функций с префиксом из имени/псевдониму данного модуля
import Data.List -- импортирование модуля целиком без ограничений

f1 = 42
f2 = undefined
f3 = ":("

