data CircList = CircList [Int] deriving Show

perimeter (CircList xs) = length xs

currentelem (CircList [])    = Nothing
currentelem (CircList (x:_)) = Just x

nextelem (CircList [])      = Nothing
nextelem (CircList [x])     = Just x
nextelem (CircList (_:xs)) = Just (head xs)

previouselem (CircList []) = Nothing
previouselem (CircList xs) = Just (last xs)

insert (CircList xs) x = CircList (xs ++ [x])

takefromCL n (CircList xs) = CircList (takefromlist n xs) where
  takefromlist n xs
   | n <= 0         = []
   | n >= length xs = xs ++ takefromlist (n - length xs) xs
   | otherwise      = take n xs

equalcirclist c1@(CircList xs) c2@(CircList ys)
 | perimeter c1 /= perimeter c2 = False
 | otherwise = tryAll (perimeter c1) xs (takefromCL (2*(perimeter c1)) c2)

tryAll 0 _ _              = False
tryAll n xs (CircList ys) = contains xs ys || tryAll (n-1) xs (CircList (tail ys))

contains [] []         = True
contains [] _          = True
contains _ []          = False
contains (x:xs) (y:ys) = x == y && contains xs ys


a = CircList [1,2,3,4,5]
b = CircList [3,4,5,1,2]
c = CircList [3,4,5,2,1]
d = CircList [3,4,5,1,2,3]











g x y = do
  a <- x
  b <- y
  return (a,b)

filter' p = foldr f [] where
 f x
  | p x       = (x:)
  | otherwise = id
