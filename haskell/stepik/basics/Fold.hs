module Test where

foldl' op b [] = b
foldl' op b (x : xs) = (foldl' op $! x + b) xs

