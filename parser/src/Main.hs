module Main (main) where
import IO
import System
import System.IO 
import System.Environment
import Lexer
import Tokens
import Parser

main = do
  args <- getArgs
  case null (args) of
    False -> do
      case (length args) < 3 of
        True -> do
          case ((args !! 0) == "-e") of
            True -> do
              analize (args !! 1)
            False -> do
              contents <- readFile (args !! 0)
              analize contents
        False -> do
          error "At most two parameters expected\nUSAGE: \tvecti file_to_analize\n\tvecti -e scrap_of_code_to_analize\n"
    True -> do
      error "At least one parameter expected\nUSAGE: \tvecti file_to_analize\n\tvecti -e scrap_of_code_to_analize\n"
  

analize x = print (parser (yylex x))
