module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving (Show)

instance Parse T where
  parse = iter Statement.parse >-> Program
  
  toString (Program []) = ""
  toString (Program (s:stmnts)) = Statement.toString s ++ "\n" ++ (toString $ Program stmnts)

exec (Program p) = Statement.exec p Dictionary.empty

--src = "read k; read n; m := 1; while n-m do begin if m - m/k*k then skip; else write m; m := m + 1; end"
