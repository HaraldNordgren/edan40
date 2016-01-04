test True True c = not c
test a True True = not a
test True b True = not b
test _ _ _       = False

g f = f.(/3).f

--e :: Num a => a -> Bool
e k = do
  x <- k
  return (2*x)
  return False
