module Stream where

import Control.Monad
import Control.Applicative

data Stream a = a :& Stream a

shead :: Stream a -> a
shead (a :& as) = a

stail :: Stream a -> Stream a
stail (a :& as) = as

smap :: (a -> b) -> Stream a -> Stream b
smap f (a :& as) = f a :& smap f as

instance Monad Stream where
	return = \a -> a :& return a
	xs >>= f = join (fmap f xs)
	    where
	      join ~(a :& as) = shead a :& join (smap stail as)

instance Applicative Stream where
	pure = return
	(<*>) = ap

instance Functor Stream where
	fmap f (a :& as) = f a :& fmap f as
