scanl' _ z []     = [z]
scanl' f z (x:xs) = z : scanl' f (f z x) xs

scanl'' f z = foldl (g f) [z]
 where g f a bs = a ++ [f (last a) bs]


scanr' _ z [] = [z]
scanr' f z xs = scanr' f (f z (last xs)) (init xs) ++ [z]

scanr'' f z = foldr g [z]
 where g a bs = f a (head bs) : bs
