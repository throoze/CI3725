{
module Parser (parser) where
import Tokens
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
num   {TkNum  _ $$}
'+'   {TkPlus a}


%%

Suma : num '+' num {  $1, $2, $3 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"


}
