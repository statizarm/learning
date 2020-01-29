module Reader where

import Kleisli

data Reader env b = Reader (env -> b)

runReader :: Reader env b -> (env -> b)
runReader (Reader f) = f

instance Kleisli (Reader env) where
	idK    = \a -> Reader (\env -> a)
	f *> g = \a -> let Reader f' = f a
                       in  Reader (\env -> let Reader g' = g . f' $ env
		                           in g' env)

instance Functor (Reader env) where
	fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader env) where
	pure                      = \a -> Reader (const a)
	(Reader f) <*> (Reader g) = Reader (\env -> f env . g $ env)

instance Monad (Reader env) where
	return    = \a -> Reader (const a)
	ma >>= fa = Reader (\env -> let a = runReader ma $ env
					b = runReader . fa $ a
				    in  b env)
