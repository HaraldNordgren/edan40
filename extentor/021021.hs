import Data.List

g x = map ($x)
g' x fs = [f x | f <- fs]

oneOf False False c = c
oneOf a False False = a
oneOf False b False = b
oneOf _ _ _         = False
