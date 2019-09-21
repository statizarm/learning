maxmin :: [Integer] -> (Integer, Integer)
maxmin lst = let h = head lst
	     in if null ( tail lst )
	        then ( h, h )
		else ( if h > t_max then h else t_max
	      	     , if h < t_min then h else t_min )
	      	     where t     = maxmin ( tail lst )
	                   t_max = fst t
	                   t_min = snd t
