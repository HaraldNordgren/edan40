module AltStringAlignment where
import Data.List

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"
type AlignmentType = (String,String)

-- Counts the score of the optimal alignment between two strings 
sim :: String -> String -> Int
sim [] [] = 0
sim string1 [] = scoreSpace * (length string1)                       
sim [] string2 = scoreSpace * (length string2)
sim (x:xs) (y:ys) = max (sim xs ys + (score x y)) (max (sim xs (y:ys)  + (score x '-')) (sim (x:xs) ys + (score '-' y)))




-- Rewritten and improved sim based on index 
mcsLength :: Eq a => [a] -> [a] -> Int
mcsLength xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry i 0 = scoreSpace * i 
    mcsEntry 0 j = scoreSpace * j 
    mcsEntry i j 
      | x == y    = scoreMatch + mcsLen (i-1) (j-1)
      | otherwise = max (scoreMismatch + mcsLen (i-1) (j-1)) (max(scoreSpace + mcsLen i (j-1)) (scoreSpace + mcsLen (i-1) j))
      where
         x = xs!!(i-1)
         y = ys!!(j-1)



score :: Char -> Char -> Int
score x y 
	| x == '-' = scoreSpace
	| y == '-' = scoreSpace
	| x == y = scoreMatch
	| otherwise = scoreMismatch




-- Appends the heads h1 h2 to every tuple in aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]




-- Returns a list of maximum values specified by the function valueFcn
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = filter (\a -> valueFcn maxValue == valueFcn a) xs
 where maxValue = maximumBy (\a b -> compare (valueFcn a) (valueFcn b)) xs




-- Returns a list of all optimal alignments between string1 and string2
optAlignments :: String -> String -> [AlignmentType]
optAlignments (s1:string1) (s2:string2) = maximaBy optScore (allAlignments (s1:string1) (s2:string2))




-- Calculate the optical score between the original tuples
optScore :: AlignmentType -> Int
optScore ([],[]) = 0
optScore (xs,[]) = scoreSpace * (length xs)
optScore ([],ys) = scoreSpace * (length ys)
optScore ((x:xs),(y:ys)) = (score x y) + optScore (xs,ys)       





-- Find all the possible alignments for the original tuples
allAlignments :: String -> String -> [AlignmentType]
allAlignments [] [] = [("", "")]
allAlignments (x:xs) [] = attachHeads x '-' (allAlignments xs [])
allAlignments [] (y:ys) = attachHeads '-' y (allAlignments [] ys)
allAlignments (x:xs) (y:ys) = (attachHeads x y (allAlignments xs ys)) ++ (attachHeads x '-' (allAlignments xs (y:ys))) ++ (attachHeads '-' y (allAlignments (x:xs) ys)) 




-- Rewritten and improved optAlignment 
betOptAlignments :: String -> String -> (Int, [AlignmentType])
betOptAlignments xs ys = betOptLength (length xs) (length ys)
 where 
  betOptLength i j = betOptTable!!i!!j
  betOptScore i j =fst  $ betOptTable!!i!!j
  betOptInfix i j =snd  $ betOptTable!!i!!j
  betOptTable = [[ betOptEntry i j | j<-[0..]] | i<-[0..] ]

  betOptEntry :: Int -> Int -> (Int, [AlignmentType])
  betOptEntry 0 0 = (0,[("","")])
  betOptEntry i 0 = (scoreSpace * i, attachTail x '-'  $ betOptInfix (i-1) 0  )
    where
         x = xs!!(i-1)
  betOptEntry 0 j = (scoreSpace * j, attachTail '-' y  $ betOptInfix 0 (j-1)  )
    where
         y = ys!!(j-1)
  betOptEntry i j	
   | x == y = (scoreMatch +  betOptScore (i-1) (j-1), attachTail x y $ betOptInfix (i-1) (j-1) )
   | otherwise =  maximaBy' fst     [(scoreMismatch + betOptScore 	(i-1) 	(j-1)	, attachTail x 	y 	$ betOptInfix (i-1) (j-1)   	),
									 (scoreSpace 	+ betOptScore 	i 		(j-1)	, attachTail '-' y  $ betOptInfix i (j-1)  		),
									 (scoreSpace 	+ betOptScore 	(i-1) 	j		, attachTail  x '-' $ betOptInfix (i-1) j			)]  
    where
         x = xs!!(i-1)
         y = ys!!(j-1)


-- New maximaBy' which fetches the previous total score
maximaBy' f x = ((fst listScore)!!0, foldl1 (++)  $ snd listScore )        
	where listScore = unzip $ maximaBy f x 
 
attachTail :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachTail h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]




-- Prints the alignments
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments [] [] = putStrLn " No optimal alignment exists for these words"
outputOptAlignments (x:xs) [] = putStrLn ("There are " ++ show (length (optAlignments (x:xs) [])) ++ "alignments" ++ "\n" ++ "\n" ++ printList (optAlignments (x:xs) []))
outputOptAlignments [] (y:ys) = putStrLn ("There are " ++ show (length (optAlignments [] (y:ys))) ++ "alignments" ++ "\n" ++ "\n" ++ printList (optAlignments [] (y:ys)))
outputOptAlignments (x:xs) (y:ys) = putStrLn ("There are " ++ show (length (optAlignments (x:xs) (y:ys))) ++ "alignments" ++ "\n" ++ "\n" ++ printList (optAlignments (x:xs) (y:ys)))




-- helper function. prints the whole list
printList :: [AlignmentType] -> String
printList [] = ""
printList [([],[])] = ""
printList (x:xs) = printPair x ++ printList xs






-- helper function, prints the pairs
printPair :: AlignmentType -> String
printPair (x,y) = x ++ "\n" ++ y ++ "\n" ++ "\n"



