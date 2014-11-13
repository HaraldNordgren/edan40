main = do  
    --foo <- putStrLn "Hello, what's your name?"  
    --name <- getLine  
    --putStrLn ("Hey " ++ name ++ ", you rock!")  

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute a [] _ = []
--substitute a b c = filter

match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard p s =
    | p[0] == wildcard = 
    | otherwise = 
