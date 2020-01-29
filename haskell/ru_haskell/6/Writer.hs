module Writer where

import Kleisli

data Writer msg b = Writer (b, msg)

instance Monoid msg => Kleisli (Writer msg) where
	idK    = \a -> Writer ((a, mempty))
	f *> j = \a -> let Writer (b, msg) = f a
	                   Writer (c, msg') = j b
		       in  Writer (c, msg `mappend` msg')

instance Monoid msg => Functor (Writer msg) where
	fmap f fa = let Writer (a, msg)  = fa
		    in  Writer (f a, msg)

instance Monoid msg => Applicative (Writer msg) where
	pure        = \a -> Writer (a, mempty)
	f <*> (Writer (a, msg)) = let (Writer (f', msg')) = f
	                              rMsg = mappend msg msg'
				  in  Writer (f' a, rMsg)

instance Monoid msg => Monad (Writer msg) where
	return = \a -> Writer (a, mempty)
	(Writer (a, msg)) >>= f = let Writer (b, msg') = f a
	                          in  Writer (b, mappend msg msg')
	                          
