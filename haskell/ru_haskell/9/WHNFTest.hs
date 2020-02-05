{-# LANGUAGE BangPatterns #-}
module WHNFTest where

data Lazy a   = Lazy a
data Strict a = Strict !a
