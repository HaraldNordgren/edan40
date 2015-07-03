f :: Eq a => [a] -> [a] -> [a]
f = filter . flip elem

f' xs ys = [y | y <- ys, elem y xs]

e k = do
  x <- k
  Nothing
  Just True
  return False

map' f = foldr ((\x -> (x:)) . f) []
map'' f = foldr ((:).f) []

g = do
  "hello"
  return "world"



data Music = Note Pitch Dur [NoteAttribute] |
             Rest Dur |
             Music :+: Music |
             Music :=: Music
type Pitch = (Cl, Int)
type Dur = Integer
type NoteAttribute = String
data Cl = C | D | E
dur = 1

instance Show Music where
  show (Note a b c) = show a
  show (Rest a)     = show a
  show (m1 :+: m2)  = show m1 ++ " " ++ show m2
instance Show Cl where
  show C = "c"
  show D = "d"
  show E = "e"

line = foldr (:+:) (Rest 0)
