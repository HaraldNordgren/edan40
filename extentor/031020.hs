replicate' n x = take n [x,x..]


h f = fst . head . dropWhile (uncurry (/=)) . ps (iterate f)
ps g x = zip (tail (g x)) (g x)


lookup' key = foldl (f key) Nothing

f _ (Just a) _ = Just a
f key Nothing (x,y)
 | key == x    = Just y
 | otherwise   = Nothing


--instance Num a => Num (Maybe a) where
-- (Just x) + (Just y) = Just (x+y)
-- _ + _               = Nothing
-- 
-- fromInteger 0 = Just 0


e k = do
  x <- k
  return (2*x)
  return False
