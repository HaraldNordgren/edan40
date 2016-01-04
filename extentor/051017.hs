import Data.List
g f x = Just (x, f x)

fmap' f m = do
  x <- m
  return $ f x

q []     = []
q (x:xs) = x : q (filter (/=x) xs)

--q' (1:[1]) = foldr f z (1:[1])
--           = f 1 (foldr f z [1])
--           = f 1 (f 1 (foldr f z []))
--           = f 1 (f 1 z)

--q' :: Eq a => [a] -> [a]
--q' = foldr f []
--f x y = x : filter (/=x) y


q' :: Eq a => [a] -> [a]
q' = foldr f []

f x xs
 | elem x xs = xs
 | otherwise = x : xs
