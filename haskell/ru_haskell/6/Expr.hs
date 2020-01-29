{-# LANGUAGE RankNTypes #-}
module Expr where

import Reader
import qualified Data.Map as M (Map, lookup, fromList)
import Control.Applicative

data Expr = Neg Expr
          | Add Expr Expr
	  | Mul Expr Expr
	  | Lit Int
	  | Var String

instance Num Expr where
	negate = Neg
	(+)    = Add
	(*)    = Mul

	fromInteger = Lit . fromInteger

	abs    = undefined
	signum = undefined

n :: Int -> Expr
n = Var . show

type Env = M.Map Key Int

type Key = String

eval :: Expr -> Reader Env Int
eval (Neg a)    = negate <$> eval a
eval (Add a b)  = liftA2 (+) (eval a) (eval b)
eval (Mul a b)  = liftA2 (*) (eval a) (eval b)
eval (Lit a)    = pure a
eval (Var name) = Reader $ \env -> value env name

value :: Env -> Key -> Int
value e k = maybe err (\x -> x) $ M.lookup k e
	where err = error $ "pshel nahoi " ++ k

runExpr :: Expr -> (Env -> Int)
runExpr e = runReader . eval $ e

