module Ast where 

data Stmt
  = Expr Expr
  | VarDecl String Expr
  | FuncDecl String [Stmt]
  | Assign String Expr 
  | If Expr [Stmt]
  | Print Expr
  deriving (Show)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | NumLit Int
  | BoolLit Bool
  | StrLit String
  | Ident String
  | Eq Expr Expr
  deriving (Show)
