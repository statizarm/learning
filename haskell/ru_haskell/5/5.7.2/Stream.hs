import Control.Category 
import qualified GHC.Base (id)
import Prelude hiding ((.), id)
infix 5 :&
data Stream a = a :& Stream a

stake :: Integer -> Stream a -> [a]
stake 0 _          = []
stake n (ah :& at) = ah : stake (n - 1) at

siterate :: (a -> a) -> a -> Stream a
siterate f a   = a :& siterate f (f a)

instance Show a => Show (Stream a) where
	show s = (\x -> init x ++ "...") . show . stake 5 $ s
	
data St a b = St (a -> (b, St a b))

ap :: St a b -> [a] -> [b]
ap _      []        = []
ap (St f) (ah : at) = let cor = f ah
	              in fst cor : ap (snd cor) at

instance Category St where
	id    = let x = \a -> (id a, St (x))
	        in  St x
	f . g = let St f' = f
	            St g' = g
		    b = fst . g'
		in  St (\a -> (fst . f' . b $ a,
		       (snd . f' $ b a) . (snd . g' $ a)))
	            

instance Num a => Num (Stream a) where
	(+) (ah :& at) (bh :& bt) = ah + bh :& at + bt
	(-) (ah :& at) (bh :& bt) = ah - bh :& at - bt
	(*) (ah :& at) (bh :& bt) = ah * bh :& at * bt
