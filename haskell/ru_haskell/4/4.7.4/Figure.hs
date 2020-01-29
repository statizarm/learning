module Figure where

import Prelude

class Figure a where
        area :: a -> Double

data Triangle   = ByThreeSide    Double Double Double
                | ByTwoSideAngle Double Double Double
	        | BySideTwoAngle Double Double Double

data Circle     = ByDiameter     Double
                | ByRadius       Double
	        | ByArc          Double

data Rectangle  = ByTwoSide      Double Double
                | BySideDiagonal Double Double


instance Figure Triangle where
	area (ByThreeSide a b c) = let p  = (a + b + c) / 2
	                               pa = p - a
				       pb = p - b
				       pc = p - c
				  in sqrt $ p * pa * pb * pc
	area (ByTwoSideAngle b c alpha) = (b * c * sin (alpha)) / 2

	area (BySideTwoAngle a alpha betta) = a * sin (alpha) * sin (betta) 
	                                   / sin (alpha + betta)

instance Figure Circle where
	area (ByDiameter d) = pi * d ^ 2 / 4
	area (ByRadius   r) = pi * r ^ 2
	area (ByArc      l) = l ^ 2 / pi

instance Figure Rectangle where
	area (ByTwoSide      a b) = a * b
	area (BySideDiagonal a c) = sqrt (c ^ 2 - a ^ 2) * a



















