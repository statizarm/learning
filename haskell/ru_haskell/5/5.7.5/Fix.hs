import Data.Function

fixMap f = let foo = \x -> if x == [] then [] else (head x :)
         
