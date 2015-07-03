import Data.List hiding (union)

data Proposition =
  Var String |
  Proposition :&: Proposition |
  Proposition :|: Proposition |
  Not Proposition

vars :: Proposition -> [String]
vars p = vars' [] p

vars' :: [String] -> Proposition -> [String] 
vars' list (Var s)
 | elem s list = list
 | otherwise   = s:list
vars' list (p1 :&: p2) = vars' (vars' list p2) p1
vars' list (p1 :|: p2) = vars' (vars' list p2) p1
vars' list (Not p) = vars' list p

truthValue :: Proposition -> [(String,Bool)] -> Bool
truthValue (Var s) dict =
  case lookup s dict of
  Just a  -> a
  Nothing -> error ("undefined variable "++s)
truthValue (p1 :&: p2) dict = truthValue p1 dict && truthValue p2 dict
truthValue (p1 :|: p2) dict = truthValue p1 dict || truthValue p2 dict
truthValue (Not p) dict = not $ truthValue p dict

allVals :: [String] -> [[(String,Bool)]]
allVals [] = [[]]
allVals (x:xs) = [(x,b):val| val <- allVals xs, b <- [False,True]]

tautology prop = tautology' prop $ allVals $ vars prop

tautology' _ [] = True
tautology' prop (v:vals) = truthValue prop v && tautology' prop vals


data Data = File Name | Directory Name [Data]
type Name = String

search :: String -> Data -> String
search query (File name)
 | query == name = name
 | otherwise     = ""
search query (Directory _ []) = ""
search query (Directory name (l:list))
 | search query l == "" = search query (Directory name list)
 | otherwise            = name++"/"++search query l


data Set a = Set [a] deriving Show
empty = Set []

add e (Set list)
 | elem e list = Set list
 | otherwise   = Set (sort $ e:list)

union (Set []) (Set [])         = empty
union (Set (a:as)) (Set [])     = add a $ Set as
union (Set []) (Set (b:bs))     = add b $ Set bs
union (Set (a:as)) (Set (b:bs)) = add a $ add b $ union (Set as) (Set bs)

remove e (Set []) = Set []
remove e (Set (l:list))
 | e == l    = Set list
 | otherwise = union (Set [l]) (remove e (Set list))

element e (Set list) = elem e list


--instance (Ord a, Ord b) => Ord (a,b) where
--  (x,y) < (z,w) = x < z || x == z && y < w

newtype ListNatural = LN [()] deriving (Show, Eq)
twoL   = LN [(),()]
threeL = LN [(),(),()]

f1 x y = foldr (:) x y
f2 x y = foldr (const (f1 x)) [] y
f3 x y = foldr (const (f2 x)) [()] y

zeroJM = LN []
zeropJM (LN x) = (x == [])
incJM (LN x) = LN (():x)
decJM (LN x) 
   | zeropJM (LN x) = error "no negative naturals exist"
   | otherwise = LN (tail x)
toIntegerJM (LN x) = (length x)

instance Num ListNatural where
   LN x + LN y = LN (x++y)
   LN x * LN y = LN (concatMap (const x) y)
   LN x - LN y
    | (length x) < (length y) = error "no negative naturals exist"
    | zeropJM (LN y) = LN x
    | otherwise = decJM (LN x) - decJM (LN y)
   abs = id
   signum x
    | zeropJM x = zeroJM
    | otherwise = incJM zeroJM
   negate = error "no negative naturals exist"
   fromInteger x 
    | x < 0  = error "no negative naturals exist"
    | x == 0 = zeroJM
    | otherwise = LN (toList x) where
        toList 0 = []
        toList n = ():(toList (n-1))
