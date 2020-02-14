{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Fix where

import Prelude hiding (succ)

newtype Fix f = Fix {unFix :: f (Fix f)}

instance Show (f (Fix f)) => Show (Fix f) where
	show x = "(" ++ show (unFix x) ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
	a == b = unFix a == unFix b

