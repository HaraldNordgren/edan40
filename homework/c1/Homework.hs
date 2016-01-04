maxi x y
 | x >= y   = x
 | otherwise = y

sumsq 1 = 1
--sumsq n = n*n + sumsq (n-1)
sumsq n = sum $ map (^2) [1..n]

--hanoi n
--  | n == 1 = 1
--  | otherwise = 1 + 2 * hanoi(n-1)
hanoi n = 2^n - 1

smallestFactor = nextFactor 1
nextFactor k n
  | mod n (k+1) == 0 = k+1
  | otherwise        = nextFactor (k+1) n

numFactors = numFactors' 0
numFactors' k n
  | k == n    = 0
  | otherwise = 1 + numFactors' (nextFactor k n) n

daysInMonth :: Month -> Year -> Integer
type Month = Integer
type Year = Integer

daysInMonth month year
  | mod year 4 == 0 && month == 2 = 29
  | month == 2                    = 28
  | elem month [1,3,5,7,8,10,12]  = 31
  | elem month [4,6,9,11]         = 30
  | otherwise                     = 0

type Date = (Integer,Month,Year)
validDate :: Date -> Bool
validDate (date,month,year) = date >= 1 && date <= daysInMonth month year

multiply :: Num a => [a] -> a
multiply [] = 1
multiply (nbr:list) = nbr * multiply list

substitute :: Char -> Char -> String -> String
substitute _ _ "" = ""
substitute a b (c:string)
  | c == a    = b:substitute a b string
  | otherwise = c:substitute a b string

duplicates [] = False
duplicates (l:list)
 | elem l list = True
 | otherwise   = duplicates list

removeDuplicates [] = []
removeDuplicates (l:list)
 | elem l list  = removeDuplicates list
 | otherwise    = l:removeDuplicates list

pairs xs ys = [(x,y) | x <- xs, y <- ys]
pythagorean n = pythagorean' (pairs [1..n] [1..n]) n
pythagorean' [] _ = []
pythagorean' ((x,y):list) n
 | x <= y && x^2+y^2 <= n = (x,y,x^2+y^2):pythagorean' list n
 | otherwise              = pythagorean' list n

isPermutation [] [] = True
isPermutation [] _ = False
isPermutation _ [] = False
isPermutation (a:as) bs
 | elem a bs = isPermutation as (remove a bs)
 | otherwise = False
remove a (b:bs)
 | a == b    = bs
 | otherwise = b:remove a bs

shortestAndLongest list = shortestAndLongest' ([],[]) list

shortestAndLongest' pair [] = pair
shortestAndLongest' (shortest,longest) (l:list)
 | length shortest == 0 && length l > length longest = shortestAndLongest' (l,l) list
 | length l < length shortest                        = shortestAndLongest' (l,longest) list
 | length l > length longest                         = shortestAndLongest' (shortest,l) list
 | otherwise                                         = shortestAndLongest' (shortest,longest) list

mystery xs = foldr1 (++) (map (\y -> [y]) xs)
