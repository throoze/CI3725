{
{-module Lexer (AlexPosn(..), alexScanTokens, yylex) where-}
module Lexer (-- * Funciones exportadas.
              -- *** yylex.
              yylex) where
import Tokens
}

%wrapper "posn"

$digit = 0-9            -- dígitos
$alpha = [a-zA-Z]        -- caracteres alfabéticos
$alphanum = [a-zA-Z0-9] -- caracteres alfanuméricos

tokens :-

  ((\"[.#\"\\\"]*\") | (\'[.#\'\\\']*\'))                { \p s -> TkStr (getPos p) (rmQuotes s)}
  $white+                                                ;
  \#.*                                                   ;
  begin                                                  { \p s -> TkBegin (getPos p) }
  in                                                     { \p s -> TkIn (getPos p) }
  num                                                    { \p s -> TkNumT (getPos p) }
  vec                                                    { \p s -> TkVec (getPos p) }
  mat                                                    { \p s -> TkMat (getPos p) }
  zeroes                                                 { \p s -> TkZeroes (getPos p) }
  range                                                  { \p s -> TkRange (getPos p) }
  eye                                                    { \p s -> TkEye (getPos p) }
  define                                                 { \p s -> TkDefine (getPos p) }
  of                                                     { \p s -> TkOf (getPos p) }
  type                                                   { \p s -> TkType (getPos p) }
  as                                                     { \p s -> TkAs (getPos p) }
  end                                                    { \p s -> TkEnd (getPos p) }
  vars                                                   { \p s -> TkVars (getPos p) }
  if                                                     { \p s -> TkIf (getPos p) }
  then                                                   { \p s -> TkThen (getPos p) }
  else                                                   { \p s -> TkElse (getPos p) }
  while                                                  { \p s -> TkWhile (getPos p) }
  do                                                     { \p s -> TkDo (getPos p) }
  read                                                   { \p s -> TkRead (getPos p) }
  write                                                  { \p s -> TkWrite (getPos p) }
  return                                                 { \p s -> TkReturn (getPos p) }
  ([Tt]rue)|(TRUE)                                       { \p s -> TkTrue (getPos p) }
  ([Ff]alse)|(FALSE)                                     { \p s -> TkFalse (getPos p) }
  foreach                                                { \p s -> TkForeach (getPos p) }
  
  \,                                                     { \p s -> TkComma (getPos p) }
  \{                                                     { \p s -> TkLBrace (getPos p) }
  \}                                                     { \p s -> TkRBrace (getPos p) }
  \(                                                     { \p s -> TkLBkt (getPos p) }
  \)                                                     { \p s -> TkRBkt (getPos p) }
  \+                                                     { \p s -> TkPlus (getPos p) }  
  \-                                                     { \p s -> TkMinus (getPos p) }
  \*\*                                                   { \p s -> TkPower (getPos p) }
  \*                                                     { \p s -> TkTimes (getPos p) }
  \/                                                     { \p s -> TkDiv (getPos p) }
  \%                                                     { \p s -> TkMod (getPos p) }
  \!\=                                                   { \p s -> TkNEqT (getPos p) }
  :=                                                     { \p s -> TkAsign (getPos p) }
  \<\=                                                   { \p s -> TkLEqT (getPos p) }
  \>\=                                                   { \p s -> TkGEqT (getPos p) }
  \=                                                     { \p s -> TkEq (getPos p) }
  \<                                                     { \p s -> TkLT (getPos p) }
  \>                                                     { \p s -> TkGT (getPos p) }
  \!                                                     { \p s -> TkNot (getPos p) }
  \:                                                     { \p s -> TkColon (getPos p) }
  \;                                                     { \p s -> TkSColon (getPos p) }
  \[                                                     { \p s -> TkLSqBkt (getPos p) }
  \]                                                     { \p s -> TkRSqBkt (getPos p) }
  \$                                                     { \p s -> TkDollar (getPos p) }
  \@                                                     { \p s -> TkAt (getPos p) }
  \^                                                     { \p s -> TkTrans (getPos p) }
  \&\&                                                   { \p s -> TkAnd (getPos p) }
  \|\|                                                   { \p s -> TkOr (getPos p) }
  
  ($digit+\.?$digit*)|($digit*\.?$digit+)                { \p s -> TkNum (getPos p) s }
  \.                                                     { \p s -> TkPoint (getPos p) }
  $alpha[$alphanum _]*                                   { \p s -> TkId (getPos p) s }

{
{- Cada una de las funciones a la derecha tiene el 
   tipo :: AlexPosn -> String -> Token 
-}


{-| La función @getPos@ se encarga de extraer la posición donde se encontró el 
   /token/ y devolverla en una tupla, para su almacenamiento en el token.
-}
getPos :: AlexPosn -> (Int,Int)
getPos (AlexPn _ f c) = (f,c)

{-| 
    Usamos la función @alexScanner@ wn lugar de alexScanToken (proveida por el wrapper
    Posn) para adaptar la lectura a nuestros requerimientos, y capturar los errores
    lexicográficos.
-}
alexScanner :: String -> ([Token], String)                           
alexScanner str = go (alexStartPos,'\n',str)
  where 
    go inp@(pos,_,str) =
       case alexScan inp 0 of
           AlexEOF                -> ([], "")
           AlexError inp'         -> concat' ([], "\nCaracter no esperado '" ++ head str : "' encontrado en linea " ++ show (fst (getPos pos)) ++ ", columna " ++ show (snd (getPos pos)) ++ ".\n") (go (alexMove pos (head str), head str, tail str))
           AlexSkip  inp' len     -> concat' ([], "") (go inp')
           AlexToken inp' len act -> concat' ([act pos (take len str)], "") (go inp')

{-|
    La función @yylex@ facilita el uso del analizador lexicográfico en un 
    programa cliente.
    
    Recibe un @String@, y devuelve una lista de /tokens/ a medida que los va 
    procesando.
-}
yylex :: String  -- ^ @String@ @s@ a procesar
      -> [Token] -- ^ Lista de /tokens/ del tipo @Token@ o lista de errores.
yylex s = do
     case null ( snd ( alexScanner s)) of
        True  -> fst ( alexScanner s)
        False -> error  (snd ( alexScanner s))

{-| 
    La función @concat'@ concatena dos tuplas @A@ y @B@ elemento a elemento.
-}
concat' :: ([x],[y])  -- ^ Tupla @A@ a concatenar
       -> ([x],[y])  -- ^ Tupla @B@ a concatenar
       -> ([x],[y])     -- ^ Tupla resultante de la concatenación
concat' a b = (fst a ++ fst b, snd a ++ snd b)

{-|
   La funcion @strToFloat@ convierte el string que hace match con un numero de
   Vectorinox en cualquier formato, y lo convierte l tipo Float. Util para tener
   el valor del número, y no su representación.
 -}
rmQuotes s = tail(init s)
           
}
