{
module Tokens_posn (AlexPosn(..), alexScanTokens, token_posn, yylex) where
import Tokens
}



%wrapper "posn"

$digit = 0-9			-- dígitos
$alpha = [a-zA-Z]		-- caracteres alfabéticos
$alphanum = [a-zA-Z0-9] -- caracteres alfanuméricos

tokens :-

  $white+((\"[\n.#\"\\\"]*\") | (\'[\n.#\'\\\']*\'))     { \p s -> TkStr getPos p s}
  $white+                                                ;
  \#.*                                                   ;
  begin                                                  { \p s -> TkBegin getPos p }
  in                                                     { \p s -> TkIn getPos p }
  num                                                    { \p s -> TkNumT getPos p }
  vec                                                    { \p s -> TkVec getPos p }
  mat                                                    { \p s -> TkMat getPos p }
  zeroes                                                 { \p s -> TkZeroes getPos p }
  range                                                  { \p s -> TkRange getPos p }
  eye                                                    { \p s -> TkEye getPos p }
  define                                                 { \p s -> TkDefine getPos p }
  of                                                     { \p s -> TkOf getPos p }
  type                                                   { \p s -> TkType getPos p }
  as                                                     { \p s -> TkAs getPos p }
  end                                                    { \p s -> TkEnd getPos p }
  vars                                                   { \p s -> TkVars getPos p }
  if                                                     { \p s -> TkIf getPos p }
  then                                                   { \p s -> TkThen getPos p }
  else                                                   { \p s -> TkElse getPos p }
  while                                                  { \p s -> TkWhile getPos p }
  do                                                     { \p s -> TkDo getPos p }
  read                                                   { \p s -> TkRead getPos p }
  write                                                  { \p s -> TkWrite getPos p }
  return                                                 { \p s -> TkReturn getPos p }
  ([Tt]rue)|(TRUE)                                       { \p s -> TkTrue getPos p }
  ([Ff]alse)|(FALSE)                                     { \p s -> TkFalse getPos p }
  foreach                                                { \p s -> TkForeach getPos p }
  
  \,                                                     { \p s -> TkComma getPos p }
  \{                                                     { \p s -> TkLBrace getPos p }
  \}                                                     { \p s -> TkRBrace getPos p }
  \(                                                     { \p s -> TkLBkt getPos p }
  \)                                                     { \p s -> TkRBkt getPos p }
  \+                                                     { \p s -> TkPlus getPos p }  
  \-                                                     { \p s -> TkMinus getPos p }
  \*\*                                                   { \p s -> TkPower getPos p }
  \*                                                     { \p s -> TkTimes getPos p }
  \/                                                     { \p s -> TkDiv getPos p }
  \%                                                     { \p s -> TkMod getPos p }
  \!\=                                                   { \p s -> TkNEqT getPos p }
  :=                                                     { \p s -> TkAsign getPos p }
  \<\=                                                   { \p s -> TkLEqT getPos p }
  \>\=                                                   { \p s -> TkGEqT getPos p }
  \<                                                     { \p s -> TkLT getPos p }
  \>                                                     { \p s -> TkGT getPos p }
  \!                                                     { \p s -> TkNot getPos p }
  \:                                                     { \p s -> TkColon getPos p }
  \[                                                     { \p s -> TkLSqBkt getPos p }
  \]                                                     { \p s -> TkRSqBkt getPos p }
  \$                                                     { \p s -> TkDollar getPos p }
  \@                                                     { \p s -> TkAt getPos p }
  \'                                                     { \p s -> TkApos getPos p }
  \&|&                                                   { \p s -> TkAnd getPos p }
  \|\|                                                   { \p s -> TkOr getPos p }
  \0                                                     { \p s -> TkEOF getPos p }
  
  ($digit+.?$digit*)|($digit*.?$digit+)                  { \p s -> TkNum getPos p s }
  \.                                                     { \p s -> TkPoint getPos p }
  $alpha[$alphanum _]*                                   { \p s -> TkId getPos p s }

{
{- Cada una de las funciones a la derecha tiene el 
   tipo :: AlexPosn -> String -> Token-}















{-|
    La función @yylex@ es facilita el uso del analizador lexicográfico
    
    Recibe un @String@, y devuelve una lista de /tokens/ a medida que los va 
    procesando.
-}
lexer :: String  -- ^ @String@ @s@ a "tokenizar"
      -> [Token] -- ^ Lista de /tokens/ del tipo @Token@ o lista de errores. 
lexer s = do
     case null ( snd ( alex_icografico s)) of   
      True  -> fst ( alex_icografico s)          
      False -> error  (snd ( alex_icografico s)) 







{-| 
    La función @alex_icografico@ que reemplaza a la funcion alexScanToken original del wrapper
    Posn para permitir el manejo de todos los errores que pueden ocurrir durante el análisis 
    lexicográfico.
-}
alex_icografico :: String -> ([Token], String)                           
alex_icografico str = go (alexStartPos,'\n',str)
  where 
    go inp@(pos,_,str) = case alexScan inp 0 of
      AlexEOF                -> ([], "")
      AlexError inp'         -> concatenar ([], "\nCaracter ilegal '" ++ head str : "' encontrado en la linea " ++ show (fst (procesarPosicion pos)) ++ ", columna " ++ show (snd (procesarPosicion pos)) ++ ".\n") (go (alexMove pos (head str), head str, tail str))
      AlexSkip  inp' len     -> concatenar ([], "") (go inp')
      AlexToken inp' len act -> concatenar ([act pos (take len str)], "") (go inp')
 

{-| 
    La función @procesarPosicion@ devuelve en forma de tupla la posición de cierto
    /token/, en el formato (línea,columna). 
-}
procesarPosicion :: AlexPosn  	-- ^ Tipo de dato definido en el wrapper Posn que se refiere a la
                      		-- ^ posición de un /token/ en el formato (AlexPn offset línea columna).
		 -> (Int, Int)  -- ^ Tupla que contiene la línea y columna de cierto /token/.
procesarPosicion (AlexPn _ f c) = (f,c)


{-| 
    La función @concatenar@ concatena cada una de las listas que conforman la tupla @A@ 
    cada una de las tuplas que conforman a la tupla @B@. 
-}
concatenar :: ([a],[b])  -- ^ Tupla @A@ a concatenar
	   -> ([a],[b])  -- ^ Tupla @B@ a concatenar
	   -> ([a],[b])	 -- ^ Tupla resultante de la concatenación
concatenar p u = (fst p ++ fst u, snd p ++ snd u)

{-
token_posn (Let p) = p
token_posn (In p) = p
token_posn (Sym p _) = p
token_posn (Var p _) = p
token_posn (Int p _) = p
-}

}
