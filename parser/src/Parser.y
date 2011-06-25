{
import Lexer
import Tokens
}

%name parser
%tokentype { Token }
%error { parseError }


%%

Pvars : Vars  Id Colon Ptype { Var $2 $4 }

Ptype : Numb {Numb $1}
      | Mat {Mat $1}
      | Vec {Vec $1}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Vars  
      = TkId Ptype
      deriving Show

data Numb
      = TkNum
      deriving Show 

data Mat
      = TkMat
      deriving Show 

data Vec
      = TkVec
      deriving Show 
}
