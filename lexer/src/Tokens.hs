{-|
  /Tokens/
	Traductores e Interpretadores CI3725

	Versión 0.1 2011-05-22

	Group H06: 
                 Victor De Ponte, 05-38087, <mailto:rdbvictor19@gmail.com>
                 Isaac López,     07-41120, <mailto:isaaclpe@gmail.com>

        Módulo desarrollado en Haskell que define el tipo Token con todos sus
        constructores, de acuerdo a la especificación del lenguaje Vectorinox.
-}
module Tokens (
  -- * Tipos exportados.
  -- ** Tokens.
  Token (..)
  ) where
  
{- El tipo de datos @Token@ modela los diferentes /Tokens/ que se pueden 
   encontrar en el lenguaje Vectorinox. Todos los /Tokens/ contienen información
   acerca de la fila y columna en que fué encontrado, en forma de tupla (f,c).
   En los casos en que sea necesaria información extra - por ejemplo TkNum, del
   cual nos interesa saber además de su posición, cuál es el número que se 
   representa (Lo mismo pasa con el String) - se añade un parámetro extra para 
   su almacenamiento.

   El tipo de datos @Token@ se declara derivando de @Show@ para que
   se pueda probar el Analizador Lexicográfico individualmente, puesto
   que al invocar la función @lexer@ la lista producida será presentada
   directamente en pantalla.

   El tipo de datos @Token@ Se declara derivando de @Eq@ para facilitar el uso de
   este tipo de datos en futuras implementaciones que lo utilicen.
-}
data Token = 
  -- Palabras Reservadas del Lenguaje
  TkNumT    (Int,Int)        |
  TkVec     (Int,Int)        |
  TkMat     (Int,Int)        |
  TkZeroes  (Int,Int)        |
  TkRange   (Int,Int)        |
  TkEye     (Int,Int)        |
  TkDefine  (Int,Int)        |
  TkOf      (Int,Int)        |
  TkType    (Int,Int)        |
  TkAs      (Int,Int)        |
  TkBegin   (Int,Int)        |
  TkEnd     (Int,Int)        |
  TkVars    (Int,Int)        |
  TkIf      (Int,Int)        |
  TkThen    (Int,Int)        |
  TkElse    (Int,Int)        |
  TkWhile   (Int,Int)        |
  TkDo      (Int,Int)        |
  TkRead    (Int,Int)        |
  TkWrite   (Int,Int)        |
  TkReturn  (Int,Int)        |
  TkTrue    (Int,Int)        |
  TkFalse   (Int,Int)        |
  TkForeach (Int,Int)        |
  TkIn      (Int,Int)        |
  
  -- Operadores y otros elementos sintácticos  
  TkComma   (Int,Int)        |
  TkSColon  (Int,Int)        |
  TkLBrace  (Int,Int)        |
  TkRBrace  (Int,Int)        |
  TkLBkt    (Int,Int)        |
  TkRBkt    (Int,Int)        |
  TkPlus    (Int,Int)        |
  TkMinus   (Int,Int)        |
  TkTimes   (Int,Int)        |
  TkDiv     (Int,Int)        |
  TkMod     (Int,Int)        |
  TkPower   (Int,Int)        |
  TkLT      (Int,Int)        |
  TkGT      (Int,Int)        |
  TkLEqT    (Int,Int)        |
  TkGEqT    (Int,Int)        |
  TkNEqT    (Int,Int)        |
  TkPoint   (Int,Int)        |
  TkLSqBkt  (Int,Int)        |
  TkRSqBkt  (Int,Int)        |
  TkDollar  (Int,Int)        |
  TkAt      (Int,Int)        |
  TkApos    (Int,Int)        |
  TkColon   (Int,Int)        |
  TkAnd     (Int,Int)        |
  TkOr      (Int,Int)        |
  TkNot     (Int,Int)        |
  TkAsign   (Int,Int)        |
  TkEOF     (Int,Int)        |
  
  --Tokens peligrosos
  TkId      (Int,Int) String |
  TkNum     (Int,Int) String Float |
  TkStr     (Int,Int) String
  deriving(Eq,Show)

