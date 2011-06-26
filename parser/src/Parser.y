

{
module Parser (parser) where
import Tokens
}

%name parser
%tokentype { Token }
%error { parserError }

%token
num        {TkNumT _}
vec        {TkVec _}
mat        {TkMat _}
zeroes     {TkZeroes _}
range      {TkRange _}
eye        {TkEye _}
define     {TkDefine _}
of         {TkOf _}
type       {TkType _}
as         {TkAs _}
begin      {TkBegin _}
end        {TkEnd _}
vars       {TkVars _}
if         {TkIf _}
then       {TkThen _}
else       {TkElse _}
while      {TkWhile _}
do         {TkDo _}
read       {TkRead _}
write      {TkWrite _}
return     {TkReturn _}
true       {TkTrue _}
false      {TkFalse _}
foreach    {TkForeach _}
in         {TkIn _}
','        {TkComma _}
';'        {TkSColon _}
'{'        {TkLBrace _}
'}'        {TkRBrace _}
'('        {TkLBkt _}
')'        {TkRBkt _}
'+'        {TkPlus _}
'-'        {TkMinus _}
'*'        {TkTimes _}
'/'        {TkDiv _}
'%'        {TkMod _}
'**'       {TkPower _}
'<'        {TkLT _}
'>'        {TkGT _}
'='        {TkEq _}
'<='       {TkLEqT _}
'>='       {TkGEqT _}
'!='       {TkNEqT _}
'.'        {TkPoint _}
'['        {TkLSqBkt _}
']'        {TkRSqBkt _}
'$'        {TkDollar _}
'@'        {TkAt _}
'^'        {TkTrans _}
':'        {TkColon _}
'&&'       {TkAnd _}
'||'       {TkOr _}
'!'        {TkNot _}
':='       {TkAsign _}
id         {TkId _ $$}
numb       {TkNum _ $$}
str        {TkStr _ $$}

%left ',' 
%%


FunVarList             : id ':' Type                                                                     { Uni $1 $3 }
                       | FunVarList ',' FunVarList                                                       { Multi $1 $3 }

Type                   : num                                                                             { Numb $1 }
                       | vec                                                                             { Vec $1 }
                       | mat                                                                             { Mat $1 }
 

{
parserError :: [Token] -> a
parserError (t:ts) = error $ 
  "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
  "Seguido de: " ++ (unlines $ map show $ take 3 ts)
  
data FunVarlist = Uni String Type
  | Multi FunVarlist FunVarlist
  deriving (Show, Eq)
   
data Type = Numb Token
  | Vec Token
  | Mat Token
  deriving (Show, Eq)

}
