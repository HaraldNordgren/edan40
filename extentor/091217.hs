import Data.List

map' f = foldr ((:).f) []

g [] = [[]]
g xs = concat [map (x:) (g (xs \\ [x])) | x <- xs]

filter' p = foldr (\x -> if p x then (x:) else id) []
