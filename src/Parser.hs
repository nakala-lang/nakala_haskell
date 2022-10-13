{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Parser (parseProgram) where

import Ast
import Control.Monad (void)
import Data.Char (isDigit, isLetter, toUpper)
import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Token (GenTokenParser (whiteSpace))
import Prelude hiding (Word, print)

ident_ :: Parser String
ident_ = do
  fc <- firstChar
  rest <- many nonFirstChar
  return (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

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

eq :: Parser Expr
eq = do
  spaces
  x <- expr
  spaces
  char '+'
  spaces
  Eq x <$> expr

num :: Parser Expr
num = do
  spaces
  n <- many1 digit
  return $ NumLit (read n)

bool :: Parser Expr
bool = do
  spaces
  n <- choice [string "true", string "false"]
  return $ BoolLit (read $ capitilizedLiteral n)
  where
    capitilizedLiteral lit = case lit of
      (fc : rest) -> toUpper fc : rest
      _ -> undefined

ident :: Parser Expr
ident = do
  spaces
  Ident <$> ident_

sstring :: Parser Expr
sstring = do
  char '"'
  s <- manyTill anyChar (char '"')
  return $ StrLit s

expr :: Parser Expr
expr = do
  spaces
  chainl1 literal binExp

binExp :: Parser (Expr -> Expr -> Expr)
binExp =
  do
    spaces
    symbol <- choice [string "+", string "-", string "=="]
    spaces
    return $ case symbol of
      "+" -> Add
      "-" -> Sub
      "==" -> Eq
      _ -> undefined

literal :: Parser Expr
literal = do
  spaces
  choice [num, bool, ident, sstring]

exprStmt :: Parser Stmt
exprStmt = do
  spaces
  Expr <$> expr

varDecl :: Parser Stmt
varDecl = do
  spaces
  string "let"
  spaces
  name <- ident_
  spaces
  char '='
  spaces
  VarDecl name <$> expr

funcDecl :: Parser Stmt
funcDecl = do
  spaces
  string "func"
  spaces
  name <- ident_
  char '('
  char ')'
  spaces
  char '{'
  body <- manyTill stmt (char '}')
  return $ FuncDecl name body

assign :: Parser Stmt
assign = do
  spaces
  name <- ident_
  spaces
  char '='
  spaces
  Assign name <$> expr

print :: Parser Stmt
print = do
  spaces
  string "print"
  char '('
  spaces
  e <- expr
  spaces
  char ')'
  return $ Print e

iff :: Parser Stmt
iff = do
  spaces
  string "if"
  spaces
  char '('
  cond <- expr
  char ')'
  spaces
  char '{'
  body <- manyTill stmt (char '}')
  return $ If cond body

stmtWithoutSemi :: Parser Stmt
stmtWithoutSemi = do
  spaces
  choice [iff, funcDecl]

stmtWithSemi :: Parser Stmt
stmtWithSemi = do
  spaces
  s <- choice [print, varDecl, assign, exprStmt]
  char ';'
  return s

stmt :: Parser Stmt
stmt = do
  spaces
  s <- choice [stmtWithoutSemi, stmtWithSemi]
  spaces
  return s

program :: Parser [Stmt]
program = do
  manyTill stmt eof

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = do
  parse program "stdin"
