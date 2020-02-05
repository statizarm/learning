module Sum where

sum2 :: [Int] -> (Int, Int)
sum2 = iter (0, 0)
	where iter res [] = res
	      iter res (a : as) = iter (tick res a) as

tick :: (Int, Int) -> Int -> (Int, Int)
tick (e, o) x | even x    = let e' = e + 1
                            in  e' `seq` (e', o)
	      | otherwise = let o' = o + 1
	                    in  o' `seq` (e, o')
