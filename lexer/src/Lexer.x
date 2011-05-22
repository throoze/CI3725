{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$alphanum = [a-zA-Z0-9]         -- alphanumeric characters

tokens :-

  $white+\"[\n.#\"\\\"]*\" { \p s -> TkStr p s}
  $white+                                                ;
  \#.*                                                   ;
  begin                                                  { \p s -> TkBegin p }
  in                                                     { \p s -> TkIn p }
  num                                                    { \p s -> TkNumT p }
  vec                                                    { \p s -> TkVec p }
  mat                                                    { \p s -> TkMat p }
  zeroes                                                 { \p s -> TkZeroes p }
  range                                                  { \p s -> TkRange p }
  eye                                                    { \p s -> TkEye p }
  define                                                 { \p s -> TkDefine p }
  of                                                     { \p s -> TkOf p }
  type                                                   { \p s -> TkType p }
  as                                                     { \p s -> TkAs p }
  end                                                    { \p s -> TkEnd p }
  vars                                                   { \p s -> TkVars p }
  if                                                     { \p s -> TkIf p }
  then                                                   { \p s -> TkThen p }
  else                                                   { \p s -> TkElse p }
  while                                                  { \p s -> TkWhile p }
  do                                                     { \p s -> TkDo p }
  read                                                   { \p s -> TkRead p }
  write                                                  { \p s -> TkWrite p }
  return                                                 { \p s -> TkReturn p }
  ([Tt]rue)|(TRUE)                                       { \p s -> TkTrue p }
  ([Ff]alse)|(FALSE)                                     { \p s -> TkFalse p }
  foreach                                                { \p s -> TkForeach p }
  
  \,                                                     { \p s -> TkComma p }
  \{                                                     { \p s -> TkLBrace p }
  \}                                                     { \p s -> TkRBrace p }
  \(                                                     { \p s -> TkLBkt p }
  \)                                                     { \p s -> TkRBkt p }
  \+                                                     { \p s -> TkPlus p }  
  \-                                                     { \p s -> TkMinus p }
  \*\*                                                   { \p s -> TkPower p }
  \*                                                     { \p s -> TkTimes p }
  \/                                                     { \p s -> TkDiv p }
  \%                                                     { \p s -> TkMod p }
  \!\=                                                   { \p s -> TkNEqT p }
  :=                                                     { \p s -> TkAsign p }
  \<\=                                                   { \p s -> TkLEqT p }
  \>\=                                                   { \p s -> TkGEqT p }
  \<                                                     { \p s -> TkLT p }
  \>                                                     { \p s -> TkGT p }
  \!                                                     { \p s -> TkNot p }
  \:                                                     { \p s -> TkColon p }
  \.                                                     { \p s -> TkPoint p }
  \[                                                     { \p s -> TkLSqBkt p }
  \]                                                     { \p s -> TkRSqBkt p }
  \$                                                     { \p s -> TkDollar p }
  \@                                                     { \p s -> TkAt p }
  \'                                                     { \p s -> TkApos p }
  \&|&                                                   { \p s -> TkAnd p }
  \|\|                                                   { \p s -> TkOr p }
  \0                                                     { \p s -> TkEOF p }
  
  $digit*.?$digit*                                       { \p s -> TkNum p s }
  $alpha[$alphanum _]*                                   { \p s -> TkId  p s }

{
-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

{- El tipo de datos @Token@ modela los diferentes /token/ que se pueden
   encontrar en el lenguaje Vectorinox.
-}
data Token = 
  -- Palabras Reservadas del Lenguaje
  TkNumT    AlexPosn         |
  TkVec     AlexPosn         |
  TkMat     AlexPosn         |
  TkZeroes  AlexPosn         |
  TkRange   AlexPosn         |
  TkEye     AlexPosn         |
  TkDefine  AlexPosn         |
  TkOf      AlexPosn         |
  TkType    AlexPosn         |
  TkAs      AlexPosn         |
  TkBegin   AlexPosn         |
  TkEnd     AlexPosn         |
  TkVars    AlexPosn         |
  TkIf      AlexPosn         |
  TkThen    AlexPosn         |
  TkElse    AlexPosn         |
  TkWhile   AlexPosn         |
  TkDo      AlexPosn         |
  TkRead    AlexPosn         |
  TkWrite   AlexPosn         |
  TkReturn  AlexPosn         |
  TkTrue    AlexPosn         |
  TkFalse   AlexPosn         |
  TkForeach AlexPosn         |
  TkIn      AlexPosn         |
  
  -- Operadores y otros elementos sintácticos  
  TkComma   AlexPosn         |
  TkSColon  AlexPosn         |
  TkLBrace  AlexPosn         |
  TkRBrace  AlexPosn         |
  TkLBkt    AlexPosn         |
  TkRBkt    AlexPosn         |
  TkPlus    AlexPosn         |
  TkMinus   AlexPosn         |
  TkTimes   AlexPosn         |
  TkDiv     AlexPosn         |
  TkMod     AlexPosn         |
  TkPower   AlexPosn         |
  TkLT      AlexPosn         |
  TkGT      AlexPosn         |
  TkLEqT    AlexPosn         |
  TkGEqT    AlexPosn         |
  TkNEqT    AlexPosn         |
  TkPoint   AlexPosn         |
  TkLSqBkt  AlexPosn         |
  TkRSqBkt  AlexPosn         |
  TkDollar  AlexPosn         |
  TkAt      AlexPosn         |
  TkApos    AlexPosn         |
  TkColon   AlexPosn         |
  TkAnd     AlexPosn         |
  TkOr      AlexPosn         |
  TkNot     AlexPosn         |
  TkAsign   AlexPosn         |
  TkEOF     AlexPosn         |
  
  --Tokens peligrosos
  TkId      AlexPosn String  |
  TkNum     AlexPosn Num     |
  TkStr     AlexPosn String  |
  deriving(Eq,Show)

token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
}
