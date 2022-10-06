{-# LANGUAGE FlexibleContexts #-}

module Parser (parseProgram) where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Prelude hiding (Word)
import Text.ParserCombinators.Parsec.Token (GenTokenParser(whiteSpace))
import Data.Char (isLetter, isDigit)

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

binExp :: Parser (Expr -> Expr -> Expr)
binExp =
  do 
    spaces
    symbol <- char '+' <|> char '-'
    spaces
    return $ case symbol of
      '+' -> Add
      '-' -> Sub
      _ -> undefined


add :: Parser Expr
add = do 
  spaces
  x <- expr
  spaces
  char '+'
  spaces
  Add x <$> expr

sub :: Parser Expr
sub = do
  spaces
  x <- expr
  spaces
  char '-'
  spaces
  Sub x <$> expr

num :: Parser Expr
num = do
  spaces
  n <- many1 digit
  return $ NumLit (read n)

expr :: Parser Expr
expr = do
  spaces
  chainl1 num binExp

exprStmt :: Parser Stmt
exprStmt = do 
  spaces
  Expr <$> expr

ident :: Parser String
ident = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

varDecl :: Parser Stmt
varDecl = do
  spaces
  string "let"
  spaces
  name <- ident
  spaces
  char '='
  spaces
  VarDecl name <$> expr
  
assign :: Parser Stmt
assign = do
  spaces
  name <- ident
  spaces
  char '='
  spaces
  Assign name <$> expr

stmt :: Parser Stmt
stmt = do
  spaces
  s <- choice [exprStmt, varDecl, assign]
  char ';'
  return s
  

program :: Parser [Stmt]
program = do 
  manyTill stmt eof

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = do
  parse program "stdin" 
