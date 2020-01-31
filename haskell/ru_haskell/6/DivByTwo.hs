module DivByTwo where

import Data.STRef
import Control.Monad.ST

divByTwo :: (Double -> Double) -> Double -> Double -> Maybe Double
divByTwo f a b = if a < b
                 then if (f a) < 0 && (f b) > 0
		      then Just $ divByTwoST (negate . f) a b
		      else if (f a) > 0 && (f b) < 0
		           then Just $ divByTwoST f a b
			   else Nothing
		 else divByTwo f b a

divByTwoST :: (Double -> Double) -> Double -> Double -> Double
divByTwoST f a b = runST $ do
	refA  <- newSTRef a
	refB  <- newSTRef b
	find f refA refB
	where find f refA refB = do
		va   <- readSTRef refA
		vb   <- readSTRef refB
		let vmid = (va + vb) / 2
		    vf   = f vmid
		if vf == 0
		then return vmid
		else if vf < 0
		     then do writeSTRef refB vmid
		             find f refA refB

		     else do writeSTRef refA vmid
		             find f refA refB

