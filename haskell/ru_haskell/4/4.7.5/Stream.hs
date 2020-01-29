module Stream where



data Stream a = a :& Stream a

instance Show a => Show (Stream a) where
	show xs = showInfinity . show $ stake 10 xs
		where showInfinity x = init x ++ "..."

intStream :: Integer -> Stream Integer
intStream a = a :& intStream (a + 1)

shead :: Stream a -> a
shead (h :& _) = h

stail :: Stream a -> Stream a
stail (_ :& t) = t

(!!!) :: Stream a -> Integer -> a
(!!!) (h :& _) 1 = h
(!!!) (h :& t) n = t !!! (n - 1)

stake :: Integer -> Stream a -> [a]
stake 0 _        = []
stake n (h :& t) = h : stake (n - 1) t

smap f (h :& t) = (f h) :& (smap f t)

sfilter :: (a -> Bool) -> Stream a -> Stream a
sfilter f (h :& t) = if f h then h :& (sfilter f t) else sfilter f t

szip :: Stream a -> Stream b -> Stream (a, b)
szip (ah :& at) (bh :& bt) = (ah, bh) :& (szip at bt)

szipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
szipWith f (ah :& at) (bh :& bt) = (f ah bh) :& (szipWith f at bt)

siterate :: (a -> a) -> a -> Stream a
siterate f a = a :& (siterate f . f $ a)
