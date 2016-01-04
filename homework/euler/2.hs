f n
 | n < 0 = 0
 | otherwise = f' n

f' 0 = 1
f' 1 = 1
f' x = f(x-1)+f(x-2)
