module QuickSort where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import Data.Array
import Data.Array.ST
import Data.Array.MArray

swapElems :: (Ix i) => STArray s i e -> i -> i -> ST s ()
swapElems arr i j = do
	vi <- readArray arr i
	vj <- readArray arr j

	writeArray arr i vj
	writeArray arr j vi

forLoop :: Ix i => i -> (i -> Bool) -> (i -> i) -> (i -> s -> s) -> s -> s
forLoop i0 pred next update s0 = runST $ do
	refI <- newSTRef i0
	refS <- newSTRef s0
	iter refI refS
	readSTRef refS
	where iter refI refS = do
		i <- readSTRef refI
		s <- readSTRef refS
		when (pred i) $ do
			writeSTRef refS $ update i s
			writeSTRef refI $ next i
			iter refI refS

quickSort :: Ord a => [a] -> [a]
quickSort val = elems $ runSTArray $ do
	arr <- newListArray (left, right) val
	quickSortST arr left right
	return arr
	where left = 0
	      right = length val - 1

quickSortST :: (Integral i, Ix i, Ord e) => STArray s i e -> i -> i -> ST s ()
quickSortST arr left right = do
	when (left <= right) $ do
		swapElems arr left $ div (left + right) 2
		vLeft     <- readArray arr left
		(last, _) <- forLoop left (<= right) succ
		                     (update vLeft) (return (left, arr))
		quickSortST arr left $ last - 1
		quickSortST arr (last + 1) right
		where update vLeft i st = do
			(last, arr) <- st
			vi          <- readArray arr i
			if vi < vLeft
		        then do	swapElems arr last i
				return (succ last, arr)
			else do return (last, arr)

	
