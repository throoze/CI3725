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

Vectorinox             : SubRoutineDef Statement                                                         {  }

SubRoutineDef          : SubRoutineDef Routine                                                           {  }
                       | {- empty -}                                                                     {  }
 
Routine                : define id '(' NullableFunVarList ')' of type Type as Statement                  {  }

NullableFunVarList     : FunVarList                                                                      {  }
                       | {- empty -}                                                                     {  }
                       
FunVarList             : id ':' Type                                                                     {  }
                       | FunVarList ',' id ':' Type                                                      {  }

Type                   : num                                                                             {  }
                       | vec                                                                             { }
                       | mat                                                                             {  }
 
Statement              : Matched                                                                         {  }
                       | Unmatched                                                                       {  }

Matched                : if BooleanExp then Matched else Matched                                         {  }
                       | Assignment                                                                      {  }
                       | Block                                                                           {  }
                       | While                                                                           {  }
                       | Foreach                                                                         {  }
                       | Read                                                                            {  }
                       | Write                                                                           {  }
                       | return                                                                          {  }

Unmatched              : if BooleanExp then Statement                                                    {  }
                       | if BooleanExp then Matched else Unmatched                                       {  }

Assignment             : Assignable ':=' Expression                                                      {  }

Assignable             : id                                                                              {  }
                       | id '[' NullableExp ':' NullableExp ']'                                          {  }
                       | id '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'          {  }
                       | id '[' Expression ']'                                                           {  }
                       | id '[' Expression ',' Expression ']'                                            {  }
                       | MatrixSector                                                                    {  }
                       | MatrixElement                                                                   {  }


Block                  : begin VarDeclarationBlock StatementList end                                     {  }

VarDeclarationBlock    : vars VarDeclarationList                                                         {  }
                       | {- empty -}                                                                     {  }

VarDeclarationList     : VarDeclarationList ';' VarList ':' Type                                         {  }
                       | VarList ':' Type                                                                {  }

VarList                : VarList ',' id                                                                  {  }
                       | id                                                                              {  }

StatementList          : StatementList ';' Statement                                                     {  }
                       | Statement                                                                       {  }


While                  : while BooleanExp do Statement                                                   {  }

Foreach                : foreach id in Expression do Statement                                           {  }

Read                   : read Assignable                                                                 {  }

Write                  : write PrintableList                                                            {  }

ExpressionList         : ExpressionList ',' Expression                                                   {  }
                       | Expression                                                                      {  }
                       
PrintableList          : PrintableList ',' Printable                                                     {  }
                       | Printable                                                                        {  }


BooleanExp             : true                                                                            {  }
                       | false                                                                           {  }
                       | '(' BooleanExp ')'                                                              {  }
                       | '!' BooleanExp                                                                  {  }
                       | BooleanExp '&&' BooleanExp                                                      {  }
                       | BooleanExp '||' BooleanExp                                                      {  }
                       | Expression '<' Expression                                                       {  }
                       | Expression '>' Expression                                                       {  }
                       | Expression '<=' Expression                                                      {  }
                       | Expression '>=' Expression                                                      {  }
                       | Expression '=' Expression                                                       {  }
                       | Expression '!=' Expression                                                      {  }
                         
Printable              : Expression                                                                      {  }
                       | str                                                                             {  }                     

                       
NullableExp            : Expression                                                                      {  }
                       | {- empty -}                                                                     {  }   


                       
RowList                :  RowList ';' ExpressionList                                                     {  }
                       | ExpressionList                                                                  {  }

Expression             : Expression '+' Expression                                                       {  }
                       | Expression '-' Expression                                                       {  }
                       | '-' Expression  %prec NEG                                                       {  }
                       | Expression '*' Expression                                                       {  }
                       | Expression '/' Expression                                                       {  }
                       | Expression '%' Expression                                                       {  }
                       | Expression '.' Expression                                                       {  }
                       | '$' Expression                                                                  {  }
                       | '@' Expression                                                                  {  }
                       | Function                                                                        {  }
                       | Function '[' NullableExp ':' NullableExp ']'                                    {  }
                       | Function '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'    {  }
                       | Function '[' Expression ']'                                                     {  }
                       | Function '[' Expression ',' Expression ']'                                      {  }
                       | '(' Expression ')'                                                              {  }
                       | numb                                                                            {  }
                       | Assignable                                                                      {  }
                       | '^' Expression                                                                  {  }
                       | Expression '**'  Expression                                                     {  }
                       | Matrix                                                                          {  }

Function               : id '(' ExpressionList ')'                                                       {  }
                       | zeroes '(' Expression ')'                                                       {  }
                       | zeroes '(' Expression ',' Expression ')'                                        {  }
                       | range '(' Expression ')'                                                        {  }
                       | eye '(' Expression ')'                                                          {  }

Matrix                 : '{' RowList '}'                                                                 {  }
                       | '{' '}'                                                                         {  }
                                              
MatrixSector           : Matrix '[' NullableExp ':' NullableExp ']'                                      {  }
                       | Matrix '[' NullableExp ':' NullableExp ',' NullableExp ':' NullableExp ']'      {  }
                       
MatrixElement          : Matrix '[' Expression ']'                                                       {  }
                       | Matrix '[' Expression ',' Expression ']'                                        {  }


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
