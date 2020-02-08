module TreeUnfold where

data Tree a   = Node a (Forest a)
	deriving (Show)

type Forest a = [Tree a]

unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b
                 in  Node a . map (unfoldTree f) $ bs

