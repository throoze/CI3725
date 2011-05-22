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
  
  -- * Funciones exportadas.
  -- *** Aqui va la descripción de una función.
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