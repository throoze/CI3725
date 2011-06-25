{
import Tokens
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    Id   {TkId $$}
    Numb {TkNum $$}
    Mat  {TkMat}
    Vec  {TkVec}
    Vars {TkVars}
    ':' {TkColon}
%%

Pvars : Vars  Id ':' Ptype { Var $2 $4 }

Ptype : Numb {Numb $1}
      | Mat {Mat $1}
      | Vec {Vec $1}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"


}
