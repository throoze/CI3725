

{
module Parser (parser) where
import Tokens
}

%name parser
%tokentype { Token }
%error { parserError }

%token
num   {TkNum  _ $$ }
'+'   {TkPlus _}
'-'   {TkMinus _}
'('   {TkLBkt _}
')'   {TkRBkt _}

%left '+'
%%

Suma : Suma '+' Suma {  Suma $1 $3 }
| num { Numero  $1 }
 
{
parserError :: [Token] -> a
parserError (t:ts) = error $ 
  "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
  "Seguido de: " ++ (unlines $ map show $ take 3 ts)
  

data ExpresionNum = Suma ExpresionNum ExpresionNum
  | Numero  String
  deriving (Eq,Show)
}
