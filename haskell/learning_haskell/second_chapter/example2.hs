(+++)::[char]->[char]->[char]
lst1 +++ lst2 = if (null lst1)
		then lst2
		else head lst1 : (tail lst1 +++ lst2)

reverse2 :: [char] -> [char]
reverse2 list = if null list
		then []
		else reverse2 (tail list) +++ [head list]
