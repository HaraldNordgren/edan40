scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore xs ys = simLen (length xs) (length ys)
  where simLen i j = simTable !!i !!j
        simTable = [[simEntry i j | j <- [0..] ] | i <- [0..]]
        simEntry i 0 = scoreSpace * i
        simEntry 0 j = scoreSpace * j
        simEntry i j = maximum [simLen (i-1) (j-1) + score x y, simLen (i-1) j + scoreSpace, simLen i (j-1) + scoreSpace]
          where x = xs !!(i-1)
                y = ys !!(j-1)
                score x y
                  | x == y    = scoreMatch
                  | otherwise = scoreMismatch

attachHeads, attachTails :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs, ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = filter isMax xs
  where isMax x = (f x >= maximum (map f xs))

--alig [] [] = [("","")]
--alig (x:xs) [] = attachHeads x '-' (alig xs "")
--alig [] (y:ys) = attachHeads '-' y (alig "" ys)
--alig (x:xs) (y:ys) = attachHeads x '-' (alig xs (y:ys)) ++ attachHeads x y (alig xs ys) ++ attachHeads '-' y (alig (x:xs) ys)

--optAlignments' :: String -> String -> [AlignmentType]
--optAlignments' s = (maximaBy simTuple) . alig s
--  where simTuple (x,y) = sim x y

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = alignment (length string1) (length string2)
  where optScore  i j = fst $ optLen i j
        alignment i j = snd $ optLen i j
        
        optLen i j = optTable !!i !!j
        optTable = [[optEntry i j | j <- [0..] ] | i <- [0..]]
        
        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry 0 0 = (0,[("","")])
        optEntry i 0 = (scoreSpace, attachTails x '-' $ alignment (i-1) 0)
          where x = string1 !!(i-1)
        optEntry 0 j = (scoreSpace, attachTails '-' y $ alignment 0 (j-1))
          where y = string2 !!(j-1)
        optEntry i j	
          | x == y    = (scoreMatch + optScore (i-1) (j-1), attachTails x y $ alignment (i-1) (j-1))
          | otherwise = nonMatch [(scoreMismatch + optScore (i-1) (j-1), attachTails x y $ alignment (i-1) (j-1)), (scoreSpace + optScore i (j-1), attachTails '-' y $ alignment i (j-1)), (scoreSpace + optScore (i-1) j, attachTails x '-' $ alignment (i-1) j)]
            where x = string1 !!(i-1)
                  y = string2 !!(j-1)

nonMatch list = (head list1, foldr1 (++) list2)
  where (list1,list2) = unzip $ maximaBy fst list

outputOptAlignments x y = do
  putStr $ "\nThere are " ++ (show $ length alignments) ++ " optimal alignments:"
  putStrLn $ divide alignments
    where alignments = optAlignments x y
          divide [] = ""
          divide (tuple:tuples) = "\n\n" ++ (fst tuple) ++ "\n" ++ (snd tuple) ++ (divide tuples)
