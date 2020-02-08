module Either where

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _  (Left a)  = fa a
        _  fb (Right b) = fb b
