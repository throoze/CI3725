{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
import Tokens
}



%wrapper "posn"

$digit = 0-9			-- dígitos
$alpha = [a-zA-Z]		-- caracteres alfabéticos
$alphanum = [a-zA-Z0-9] -- caracteres alfanuméricos

tokens :-

  $white+((\"[\n.#\"\\\"]*\") | (\'[\n.#\'\\\']*\'))     { \p s -> TkStr p s}
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
  \[                                                     { \p s -> TkLSqBkt p }
  \]                                                     { \p s -> TkRSqBkt p }
  \$                                                     { \p s -> TkDollar p }
  \@                                                     { \p s -> TkAt p }
  \'                                                     { \p s -> TkApos p }
  \&|&                                                   { \p s -> TkAnd p }
  \|\|                                                   { \p s -> TkOr p }
  \0                                                     { \p s -> TkEOF p }
  
  ($digit+.?$digit*)|($digit*.?$digit+)                  { \p s -> TkNum p s }
  \.                                                     { \p s -> TkPoint p }
  $alpha[$alphanum _]*                                   { \p s -> TkId  p s }

{
-- Cada una de las funciones a la derecha tiene el tipo :: AlexPosn -> String -> Token
{-
token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
-}

}
