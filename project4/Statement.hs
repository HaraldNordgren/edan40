module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show


assignment = (word #- accept ":=") # (Expr.parse #- require ";") >-> buildAss
  where buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
  where buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

ifs = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
  where buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
  where buildWhile (e,s) = While e s

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write


exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment var e:stmts) dict input = exec stmts (Dictionary.insert (var, Expr.value e dict) dict) input
exec (Skip:stmts) dict input = exec stmts dict input
exec (Begin cs:stmts) dict input = exec (cs ++ stmts) dict input
exec (If cond thenStmt elseStmt:stmts) dict input = 
    if Expr.value cond dict > 0 
    then exec (thenStmt:stmts) dict input
    else exec (elseStmt:stmts) dict input
exec (While cond s:stmts) dict input =
    if Expr.value cond dict > 0
    then exec (s:While cond s:stmts) dict input
    else exec stmts dict input
exec (Read s:_) _ [] = error "too few input arguments"
exec (Read s:stmts) dict (nbr:input) = exec stmts (Dictionary.insert (s, nbr) dict) input
exec (Write e:stmts) dict input = Expr.value e dict:exec stmts dict input


toString' ind (Assignment s e) = ind++s++" := "++toString e++";"
toString' ind Skip = ind++"skip;"
toString' ind (Begin stmnts) = ind++"begin\n"++helper++ind++"end"
  where helper = foldr1 (++) $ map ((++"\n") . toString' (ind++twoSpaces)) stmnts
toString' ind (If e s1 s2) = ind++"if "++toString e++" then\n"++ toString' (ind++twoSpaces) s1 ++"\n"++ind++"else\n"++ toString' (ind++twoSpaces) s2
toString' ind (While e s) = ind++"while "++toString e++" do\n"++ toString' (ind++twoSpaces) s
toString' ind (Read string) = ind++"read "++string++";"
toString' ind (Write e) = ind++"write "++toString e++";"

twoSpaces = "  "

instance Parse Statement where
  parse = assignment ! skip ! begin ! ifs ! while ! read ! write
  toString e = toString' "" e
