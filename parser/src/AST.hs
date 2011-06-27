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

data Routine = Token [Uni] Type Statement
     deriving (Show, Eq)

data VARS = Uni id Type
     deriving (Show, Eq)
   
data Type = Numb Token
  | Vec Token
  | Mat Token
  deriving (Show, Eq)

data Statement = MBoolean BooleanExp Statement Statement
  | BooleanS BooleanExp Statement
  | UBoolean BooleanExp Statement Statement
  | Assign Assignable Expression
  | Block [Declaration] [Statement]
  | While BooleanExp Statement
  | FE id Expression Statement
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
  | FuncVR Function Expression Expression
  | BRE Expression
  | TRP Expression
  | POW Expression Expression
  deriving (Show, Eq)

data Function = Func [Expression]
  | ZeroesV Expression
  | ZeroesM Expression
  | Range Expression
  | Eye Expression
  deriving (Show, Eq)

data Matrix = Mat [Expression]
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

data Dec = Declaration  [id] Type
     deriving (Show, Eq)
