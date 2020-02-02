module Compose where

import Control.Applicative

data Hmm f g a = Hmm {runHmm :: f (g a)} 
	deriving (Show)

instance (Functor f, Functor g) => Functor (Hmm f g) where
	fmap f = \a -> Hmm $ fmap (fmap f) $ runHmm a

instance (Applicative f, Applicative g) => Applicative (Hmm f g) where
	pure    = \a -> Hmm $ pure . pure $ a
	f <*> a = Hmm $ liftA2 (<*>) (runHmm f) (runHmm a)

