module Test where

coins = [2, 3, 7]

change a | a <= 0 = [[]]
change a = filter (\x -> sum x == a) . concat $ [map (x:) $ change (a - x) | x <- coins]
