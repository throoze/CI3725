module AST (
  POG(..),
  Routine(..),
  VARS(..),
  Type(..),
  Statement(..),
  Printable(..),
  Expression(..),
  Function(..),
  Matrix(..),
  Sector(..),
  Elem(..),
  Assignable(..),
  Dec(..),
  ) where

data POG = Vectorinox [Routine] Statement
         deriving (Show, Eq) 
                  
data Rout = Routine  Token [VARS] Type Statement
             deriving (Show, Eq)
                      
data VARS = Uni Token Type
          deriving (Show, Eq)
                   
data Type = Numb Token
          | Vec Token
          | Mat Token
          deriving (Show, Eq)
                   
data Statement = MBoolean Statement Statement Statement
               | BooleanS Statement Statement
               | UBoolean Statement Statement Statement
               | Assign Assignable Expression
               | Block [Dec] [Statement]
               | While Statement Statement
               | FE Token Expression Statement
               | READ Assignable
               | WRITE  Printable
               deriving (Show, Eq)
                        
data Printable = PrintE Expression
               | PrintS Token
               deriving (Show, Eq)
                        
data Expression = PL Expression Expression
                | MI Expression Expression
                | MIU Expression
                | Times Expression Expression
                | DIV Expression Expression
                | MOD Expression Expression
                | PNT Expression Expression
                | ROW Expression
                | COL Expression
                | FuncVR Function Maybe Expression Maybe Expression
                | FuncMR Function Maybe Expression Maybe Expression Maybe Expression Maybe Expression
                | FuncVP Function Expression
                | FuncMP Function Expression Expression
                | BRE Expression
                | TRP Expression
                | POW Expression Expression
                | Bc Expression
                | Neg Expression
                | AND Expression Expression
                | OR Expression
                | LT Expression Expression
                | GT Expression Expression
                | LE Expression Expression
                | GE Expression Expression
                | Eq Expression Expression
                | NEq Expression Expression
                deriving (Show, Eq)
                         
data Function = Func [Expression]
              | ZeroesV Expression
              | ZeroesM Expression
              | Range Expression
              | Eye Expression
              deriving (Show, Eq)
                       
data Matrix = Matr [Expression]
            | EmptyMat
            deriving (Show, Eq)
                     
data Sector = SectorV Matrix Maybe Expression Maybe Expression
            | SectorM Matrix Maybe Expression Maybe Expression Maybe Expression Maybe Expression
            deriving (Show, Eq)
                     
data Elem = ElemM Matrix Expression
          | ElemV Matrix Expression
          deriving (Show, Eq)
                   
data Assignable = AssignableRV Token Expression Expression
                | AssignableRM Token Expression Expression Expression Expression
                | AssignablePV Token Expression
                | AssignableMV Token Expression Expression
                deriving (Show, Eq)
                         
data Dec = Declaration  [Token] Type
         deriving (Show, Eq)