module Tree where

data Tree a = Node a [Tree a]

instance Functor Tree where
	fmap f (Node a as) = Node (f a) (map (fmap f) as)

instance Applicative Tree where
	pure = \a -> Node a []
	(Node f fs) <*> (Node a as) = (Node (f a) (zipWith (<*>) fs as))

