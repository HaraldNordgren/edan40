module Pattern where
import Utilities

--frenchPresentation = ("My name is *", "Je m'appelle *")
--swedishPresentation = [("I live in *", "Jag bor i *"), ("My name is *", "Jag heter *"), ("I am from *", "Jag kommer från *")] 


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (p:ps) s
    | p == wildcard = substitute wildcard (s ++ ps) s
    | otherwise = p:(substitute wildcard ps s)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard (p:ps) (s:ss)
    | (p == wildcard) = orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))
    | p == s = match wildcard ps ss
    | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const[x]) (match wc ps xs)

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f string (x, y) = mmap ((substitute wc y) . f)  (match wc x string)


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f tuples string = foldr1 orElse $ map (transformationApply wc f string) tuples
