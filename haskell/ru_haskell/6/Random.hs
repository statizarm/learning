module Random where

import State
import Control.Applicative

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 *)

type Random s = State Double s

next :: Random Double
next = State (\s -> (s, nextRandom s))

addRandom :: Double -> Random Double
addRandom x = fmap (+ x) next

addRandom2 :: Double -> Double -> Random Double
addRandom2 a b = liftA2 add next next
	where add x y  = diap a 1 x + diap b 1 y
	      diap c r = \x -> (x - 0.5) * 2 * r + c


data Coin = Face | Back
	deriving (Show)

dropCoin :: Random Coin
dropCoin = fmap (\x -> if x > 0.5 then Face else Back) next
