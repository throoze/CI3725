

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

%left '+' '-'
%%

Vectorinox             : SubRoutineDef Statement                                                         {  }

SubRoutineDef          : SubRoutineDef define id '(' FunVarList ')' of type Type as Statement            {  }
                       | {- empty -}                                                                     { [] }

FunVarList             : FunVarsList id ':' Type                                                         {  }
                       | {- empty -}                                                                     { [] }

FunVarsList            : FunVarsList id ':' Type ','                                                     {  }
                       | {- empty -}                                                                     { [] }

Type                   : num                                                                             {  }
                       | vec                                                                             {  }
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

Unmatched              : if BooleanExp then Statement                                                    {  }
                       | if BooleanExp then Matched else Unmatched                                       {  }

Assignment             : Assignable ':=' Expression                                                      {  }

Assignable             : id                                                                              {  }
                       | VectorElement                                                                   {  }
                       | MatrixElement                                                                   {  }
                       | VectorSector                                                                    {  }
                       | MatrixSector                                                                    {  }

VectorElement          : Expression '[' numb ']'                                                         {  }

MatrixElement          : Expression '[' numb ',' numb ']'                                                {  }

VectorSector           : Expression '[' Coordinate ':' Coordinate ']'                                    {  }

MatrixSector           : Expression '[' Coordinate':' Coordinate ',' Coordinate ':' Coordinate ']'       {  }

Coordinate             : numb                                                                            {  }
                       | {- empty -}                                                                     { [] }

Block                  : begin VarDeclaration StatementList end                                          {  }

VarDeclaration         : vars VarDeclarationList                                                         {  }
                       | {- empty -}                                                                     { [] }

VarDeclarationList     : Declarations Identifier ':' Type                                                {  }

Declarations           : Declarations Identifier ':' Type ';'                                            {  }
                       | {- empty -}                                                                     { [] }

Identifier             : Identifiers id                                                                  {  }

Identifiers            : Identifiers id ','                                                              {  }
                       | {- empty -}                                                                     { [] }

StatementList          : StatementsList Statement                                                        {  }

StatementsList         : StatementsList Statement ';'                                                    {  }
                       | {- empty -}                                                                     { [] }

While                  : while BooleanExp do Statement                                                   {  }

Foreach                : foreach id in Expression do Statement                                           {  }

Read                   : read Assignable                                                                 {  }

Write                  : write ExpressionList                                                            {  }

ExpressionList         : Expressions Expression                                                          {  }

Expressions            : Expressions Expression ','                                                      {  }
                       | {- empty -}                                                                     { [] }

Expression             : BooleanExp                                                                      {  }
                       | NonBooleanExp                                                                   {  }

BooleanExp             : ShortCircuitAnd                                                                 {  }
                       | ShortCircuitOr                                                                  {  }
                       | Negation                                                                        {  }
                       | Comparison                                                                      {  }
                       | true                                                                            {  }
                       | false                                                                           {  }

ShortCircuitAnd        : BooleanExp '&&' BooleanExp                                                      {  }
                       
ShortCircuitOr         : BooleanExp '||' BooleanExp                                                      {  }

Negation               : '!' BooleanExp                                                                  {  }

Comparison             : LessThan                                                                        {  }
                       | GreaterThan                                                                     {  }
                       | LessEqThan                                                                      {  }
                       | GreaterEqThan                                                                   {  }
                       | Equal                                                                           {  }
                       | Unequal                                                                         {  }

LessThan               : Expression '<' Expression                                                       {  }

GreaterThan            : Expression '>' Expression                                                       {  }

LessEqThan             : Expression '<=' Expression                                                      {  }

GreaterEqThan          : Expression '>=' Expression                                                      {  }

Equal                  : Expression '=' Expression                                                       {  }

Unequal                : Expression '!=' Expression                                                      {  }

NonBooleanExp          : Matrix                                                                          {  }
                       | Operation                                                                       {  }
                       | Assignable                                                                      {  }
                       | str                                                                          {  }

Matrix                 : '{' RowList '}'                                                                 {  }
                       | VectorSector                                                                    {  }
                       | MatrixSector                                                                    {  }

RowList                :  RowsList ExpressionList                                                        {  }

RowsList               :  RowsList ExpressionList ';'                                                    {  }
                       | {- empty -}                                                                     { [] }

Operation              : Expression '+' Term                                                             {  }
                       | Expression '-' Term                                                             {  }
                       | Term                                                                            {  }
                       | FunctionCall                                                                    {  }

Term                   : Term '*' Factor                                                                 {  }
                       | Term '/' Factor                                                                 {  }
                       | Term '%' Factor                                                                 {  }
                       | Factor                                                                          {  }


Factor                 : '-' Atom                                                                        {  }
                       | Atom                                                                            {  }

Atom                   : '$' Matrix                                                                      {  }
                       | '@' Matrix                                                                      {  }
                       | '^' Matrix                                                                      {  }
                       | id '(' ExpressionList ')'                                                       {  }
                       | '(' Expression ')'                                                              {  }
                       | Matrix                                                                          {  }
                       | Power                                                                           {  }
                       | numb                                                                            {  }
                       | id                                                                              {  }

Power                  : '**' '(' Expression ')'                                                         {  }
                       | '**' id '(' ExpressionList ')'                                                  {  }
                       | '**' numb                                                                       {  }

 
{
parserError :: [Token] -> a
parserError (t:ts) = error $ 
  "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
  "Seguido de: " ++ (unlines $ map show $ take 3 ts)
  

data ExpresionNum = Suma ExpresionNum ExpresionNum
  | Numero  String
  deriving (Eq,Show)
}
