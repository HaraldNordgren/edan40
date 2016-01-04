data Digits = Zero | One | Two | Three | Four | Six | Seven | Eight | Nine deriving (Enum,Show)

smallDigits = [Zero .. Three]

f x y = do
  a <- x
  b <- y
  return (a*b)

--replicate' :: Enum a => Int -> a -> [a]
replicate' n x = take n [x, x ..]
