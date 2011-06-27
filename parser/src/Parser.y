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

%left if else
%left '||'
%left '&&'
%left '!'
%nonassoc '<' '>' '<=' '>=' '=' '!='
%left '+' '-'
%left '*' '/' '.' '%'
%left '$' '@'
%left '^'
%left NEG
%right '**'
%left ';'
%left ','
%nonassoc '(' ')' '{' '}' '[' ']'

%%

Vectorinox             : SubRoutineDef Statement                                                         { {-Vectorinox $1 $2 -}}

SubRoutineDef          : SubRoutineDef Routine                                                           { {- $1 ++ [$2] -}}
                       | {- empty -}                                                                     { {-Nothing -}}
 
Routine                : define id '(' NullableFunVarList ')' of type Type as Statement                  { {-Routine $2 $4 $8 $10 -}}

NullableFunVarList     : FunVarList                                                                      { {- $1 -}}
                       | {- empty -}                                                                     { {-Nothing -}}
                       
FunVarList             : id ':' Type                                                                     { {-[Uni $1 $3] -}}
                       | FunVarList ',' id ':' Type                                                      { {- $1 ++ [Uni $3 $5] -}}

Type                   : num                                                                             { {-Numb $1 -}}
                       | vec                                                                             { {-Vec $1 -}}
                       | mat                                                                             { {-Mat $1 -}}
 
Statement              : Matched                                                                         { {- $1 -}}
                       | Unmatched                                                                       { {- $1 -}}

Matched                : if BooleanExp then Matched else Matched                                         { {-MBoolean $2 $4 $6 -}}
                       | Assignment                                                                      { {- $1 -}}
                       | Block                                                                           { {- $1 -}}
                       | While                                                                           { {- $1 -}}
                       | Foreach                                                                         { {- $1 -}}
                       | Read                                                                            { {- $1 -}}
                       | Write                                                                           { {- $1 -}}
                       | return                                                                          { {- $1 -}}

Unmatched              : if BooleanExp then Statement                                                    { {-BooleanS $2 $4 -}}
                       | if BooleanExp then Matched else Unmatched                                       { {-UBoolean $2 $4 $6 -}}

Assignment             : Assignable ':=' Expression                                                      { {-Assign $1 $3 -}}

Assignable             : id                                                                              { {- $1 -}}
                       | id '[' NullableExp ':' NullableExp ']'                                          { {-AssignableRV $1 $3 $5 -}}
                       | id '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'          { {-AssignableRM $1 $3 $5 $7 $9 -}}
                       | id '[' Expression ']'                                                           { {-AssignablePV $1 $3 -}}
                       | id '[' Expression ',' Expression ']'                                            { {-AssignableMV $1 $3 $5-}}
                       | MatrixSector                                                                    { {- $1 -}}
                       | MatrixElement                                                                   { {- $1 -}}


Block                  : begin VarDeclarationBlock StatementList end                                     { {-Block $2 $3 -}}

VarDeclarationBlock    : vars VarDeclarationList                                                         { {- $2 -}}
                       | {- empty -}                                                                     { {-Nothing -}}

VarDeclarationList     : VarDeclarationList ';' VarList ':' Type                                         { {- $1 ++ [Declaration $3 $5] -}}
                       | VarList ':' Type                                                                { {-[Declaration $1 $2] -}}

VarList                : VarList ',' id                                                                  { {- $1 ++ [$3] -}}
                       | id                                                                              { {- [$1] -}}

StatementList          : StatementList ';' Statement                                                     { {- $1 ++ [$3] -}}
                       | Statement                                                                       { {-[$1] -}}


While                  : while BooleanExp do Statement                                                   { {-While $2 $4 -}}

Foreach                : foreach id in Expression do Statement                                           { {-FE $2 $4 $6 -}}

Read                   : read Assignable                                                                 { {-READ $2 -}}

Write                  : write PrintableList                                                             { {-WRITE $2 -}}

ExpressionList         : ExpressionList ',' Expression                                                   { {- $1 ++ [$3] -}}
                       | Expression                                                                      { {-[$1] -}}
                       
NullableExpressionList : ExpressionList                                                                  { {- $1 -}}
                       | {- empty -}                                                                     { {-Nothing -}}                   
                       
PrintableList          : PrintableList ',' Printable                                                     { {- $1 ++ [$3] -}}
                       | Printable                                                                       { {-[$1] -}}


BooleanExp             : true                                                                            { {- $1 -}}
                       | false                                                                           { {- $1 -}}
                       | '(' BooleanExp ')'                                                              { {-Bc $2 -}}
                       | '!' BooleanExp                                                                  { {-Neg $1 -}}
                       | BooleanExp '&&' BooleanExp                                                      { {-AND $1 -}}
                       | BooleanExp '||' BooleanExp                                                      { {-OR $1 -}}
                       | Expression '<' Expression                                                       { {-LoT $1 $3 -}}
                       | Expression '>' Expression                                                       { {-GeT $1 $3 -}}
                       | Expression '<=' Expression                                                      { {-LE $1 $3 -}}
                       | Expression '>=' Expression                                                      { {-GE $1 $3 -}}
                       | Expression '=' Expression                                                       { {-Eq $1 $3 -}}
                       | Expression '!=' Expression                                                      { {-NEq $1 $3 -}}
                         
Printable              : Expression                                                                      { {-PrintE $1 -}}
                       | str                                                                             { {-PrintS $1 -}}                     

                       
NullableExp            : Expression                                                                      { {- $1 -}}
                       | {- empty -}                                                                     { {- Nothing -}}   


                       
RowList                : RowList ';' ExpressionList                                                      { {- $1 ++ [$3] -}}
                       | ExpressionList                                                                  { {-[$1] -}}

Expression             : Expression '+' Expression                                                       { {-PL $1 $3 -}}
                       | Expression '-' Expression                                                       { {-MI $1 $3 -}}
                       | '-' Expression  %prec NEG                                                       { {-MIU $1 -}}
                       | Expression '*' Expression                                                       { {-Times $1 $3 -}}
                       | Expression '/' Expression                                                       { {-DIV $1 $3 -}}
                       | Expression '%' Expression                                                       { {-MOD $1 $3 -}}
                       | Expression '.' Expression                                                       { {-PNT $1 $3 -}}
                       | '$' Expression                                                                  { {-ROW $1 -}}
                       | '@' Expression                                                                  { {-COL $1 -}}
                       | Function                                                                        { {- $1 -} }
                       | Function '[' NullableExp ':' NullableExp ']'                                    { {-FuncVR $1 $3 $5 -}}
                       | Function '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'    { {-FuncMR $1 $3 $5 $7 $9-}}
                       | Function '[' Expression ']'                                                     { {-FuncVP $1 -}}
                       | Function '[' Expression ',' Expression ']'                                      { {-FuncMP $1 $3 $5 -}}
                       | '(' Expression ')'                                                              { {-BRE $2 -}}
                       | numb                                                                            { {- $1 -}}
                       | Assignable                                                                      { {- $1 -}}
                       | '^' Expression                                                                  { {-TRP $2 -}}
                       | Expression '**'  Expression                                                     { {-POW $1 $3 -}}
                       | Matrix                                                                          { {- $1 -}}

Function               : id '(' NullableExpressionList ')'                                               { {-Func $3 -}}
                       | zeroes '(' Expression ')'                                                       { {-ZeroesV $3 -}}
                       | zeroes '(' Expression ',' Expression ')'                                        { {-ZeroesM $3 $5 -}}
                       | range '(' Expression ')'                                                        { {-Range $3 -}}
                       | eye '(' Expression ')'                                                          { {-Eye $3 -}}

Matrix                 : '{' RowList '}'                                                                 { {-Matr $2 -}}
                       | '{' '}'                                                                         { {-EmptyMat -}}
                                              
MatrixSector           : Matrix '[' NullableExp ':' NullableExp ']'                                      { {-SectorV $1 $3 $ 5 -}}
                       | Matrix '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'      { {-SectorM $1 $3 $ 5 -}}
                       
MatrixElement          : Matrix '[' Expression ']'                                                       { {-ElemM $1 $3 -}}
                       | Matrix '[' Expression ',' Expression ']'                                        { {-ElemV $1 $3 -} }


{
parserError :: [Token] -> a
parserError (t:ts) = error $ 
  "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
  "Seguido de: " ++ (unlines $ map show $ take 3 ts)

{-
data POG = Vectorinox [Rout] Statement
         deriving (Show, Eq) 
                  
data Rout = Routine  Token [VARS] Type Statement
             deriving (Show, Eq)
                      
data VARS = Uni Token Type
          deriving (Show, Eq)
                   
data Type = Numb String
          | Vec String
          | Mat String
          deriving (Show, Eq)
                   
data Statement = MBoolean Statement Statement Statement
               | BooleanS Statement Statement
               | UBoolean Statement Statement Statement
               | Assign Assignable Expression
               | Block [Dec] [Statement]
               | While Statement Statement
               | FE Token Expression Statement
               | READ Assignable
               | WRITE  Printable
               deriving (Show, Eq)
                        
data Printable = PrintE Expression
               | PrintS Token
               deriving (Show, Eq)
                        
data Expression = PL Expression Expression
                | MI Expression Expression
                | MIU Expression
                | Times Expression Expression
                | DIV Expression Expression
                | MOD Expression Expression
                | PNT Expression Expression
                | ROW Expression
                | COL Expression
                | FuncVR Function (Maybe Expression) (Maybe Expression)
                | FuncMR Function (Maybe Expression) (Maybe Expression) (Maybe Expression) (Maybe Expression)
                | FuncVP Function Expression
                | FuncMP Function Expression Expression
                | BRE Expression
                | TRP Expression
                | POW Expression Expression
                | Bc Expression
                | Neg Expression
                | AND Expression Expression
                | OR Expression
                | LoT Expression Expression
                | GeT Expression Expression
                | LE Expression Expression
                | GE Expression Expression
                | Eq Expression Expression
                | NEq Expression Expression
                deriving (Show, Eq)
                         
data Function = Func [Expression]
              | ZeroesV Expression
              | ZeroesM Expression
              | Range Expression
              | Eye Expression
              deriving (Show, Eq)
                       
data Matrix = Matr [Expression]
            | EmptyMat
            deriving (Show, Eq)
                     
data Sector = SectorV Matrix (Maybe Expression) (Maybe Expression)
              | SectorM Matrix (Maybe Expression) (Maybe Expression) (Maybe Expression) (Maybe Expression)
            deriving (Show, Eq)
                     
data Elem = ElemM Matrix Expression
          | ElemV Matrix Expression
          deriving (Show, Eq)
                   
data Assignable = AssignableRV Token Expression Expression
                | AssignableRM Token Expression Expression Expression Expression
                | AssignablePV Token Expression
                | AssignableMV Token Expression Expression
                deriving (Show, Eq)
                         
data Dec = Declaration  [Token] Type
         deriving (Show, Eq)

-}
}
