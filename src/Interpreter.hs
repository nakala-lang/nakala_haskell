{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast
import Value
import Control.Monad (when)

eval :: [Stmt] -> IO ()
eval =
  mapM_ evalStmt

evalStmt :: Stmt -> IO ()
evalStmt stmt =
  case stmt of
    Print e -> do
      val <- evalExpr e
      printValue val
    If c body -> do
      cond <- evalExpr c
      when (isTruthy cond) $ eval body
    Expr e -> do
      evalExpr e
      return ()
    _ -> undefined

evalExpr :: Expr -> IO Value
evalExpr e =
  case e of
    NumLit n -> return $ Num n
    StrLit s -> return $ Str s
    BoolLit b -> return $ Bool b
    Add x y -> do
      a <- evalExpr x
      b <- evalExpr y
      return $ addValues a b
    Sub x y -> do
      a <- evalExpr x
      b <- evalExpr y
      return $ subValues a b
    _ -> undefined


