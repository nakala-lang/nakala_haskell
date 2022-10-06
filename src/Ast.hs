module Ast where 

data Stmt
  = Expr Expr
  | VarDecl String Expr
  | Assign String Expr 
  deriving (Show)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | NumLit Int
  | Eq Expr Expr
  deriving (Show)
