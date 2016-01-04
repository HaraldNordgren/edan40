--g x = map ($x)
g x list = [y$x | y <- list]

oneOf False False c = c
oneOf False b False = b
oneOf a False False = a
oneOf _ _ _         = False

f = do [3]
       "lambda"
