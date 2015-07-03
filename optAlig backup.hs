optAlignments' :: String -> String -> (Int, [AlignmentType])
optAlignments' xs ys = optLen (length xs) (length ys)
  where optLen i j = optTable!!i!!j
        betOptScore i j = fst $ optTable!!i!!j
        betOptInfix i j = snd $ optTable!!i!!j
        optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]
        
        
        optEntry :: Int -> Int -> (Int, [AlignmentType])
        optEntry 0 0 = (0,[("","")])
        optEntry i 0 = (scoreSpace * i, attachTails x '-'  $ betOptInfix (i-1) 0  )
          where x = xs!!(i-1)
        optEntry 0 j = (scoreSpace * j, attachTails '-' y  $ betOptInfix 0 (j-1)  )
          where y = ys!!(j-1)
        optEntry i j	
          | x == y    = (scoreMatch +  betOptScore (i-1) (j-1), attachTails x y $ betOptInfix (i-1) (j-1) )
          | otherwise =  maximaBy' fst [(scoreMismatch + betOptScore (i-1) (j-1), attachTails x y $ betOptInfix (i-1) (j-1)), (scoreSpace + betOptScore i (j-1), attachTails '-' y  $ betOptInfix i (j-1)), (scoreSpace + betOptScore (i-1) j, attachTails  x '-' $ betOptInfix (i-1) j)] where
                           x = xs!!(i-1)
                           y = ys!!(j-1)


-- New maximaBy' which fetches the previous total score
maximaBy' f x = ((fst listScore)!!0, foldl1 (++)  $ snd listScore )        
	where listScore = unzip $ maximaBy f x
