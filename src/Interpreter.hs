{-# LANGUAGE TupleSections #-}

module Interpreter where

import Ast
import Control.Monad (when)
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Value

data Env = Env
  { vars :: M.Map String Value,
    funcs :: M.Map String Value
  }

lookupVar :: Monad m => String -> StateT Env m (Maybe Value)
lookupVar name = do
  env <- get
  let m = vars env
   in return $ M.lookup name m

declareVar :: Monad m => String -> Value -> StateT Env m ()
declareVar name value = do
  env <- get
  let m = vars env
      m' = M.insert name value m
   in put env {vars = m'}

lookupFunc :: Monad m => String -> StateT Env m (Maybe Value)
lookupFunc name = do
  env <- get
  let m = funcs env
   in return $ M.lookup name m

declareFunc :: Monad m => String -> Value -> StateT Env m ()
declareFunc name value = do
  env <- get
  let m = funcs env
      m' = M.insert name value m
   in put env {funcs = m'}

eval :: [Stmt] -> IO ()
eval stmts = do
  (res, _) <- runStateT (evalBlock stmts) Env {vars = M.empty, funcs = M.empty}
  return res

evalBlock :: [Stmt] -> StateT Env IO ()
evalBlock stmts = do
  mapM_ evalStmt stmts

evalStmt :: Stmt -> StateT Env IO ()
evalStmt stmt = do
  case stmt of
    Print e -> do
      val <- evalExpr e
      liftIO $ printValue val
    If c body -> do
      cond <- evalExpr c
      when (isTruthy cond) $ do
        evalBlock body
    VarDecl name value -> do
      val <- evalExpr value
      declareVar name val
    FuncDecl name body -> do
      let func = Func name body
      declareFunc name func
    Expr e -> do
      evalExpr e
      return ()
    _ -> undefined

evalExpr :: Monad m => Expr -> StateT Env m Value
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
    Ident name -> do
      res <- lookupVar name
      case res of
        Just x -> return x
        _ -> do
          return $ print "Not found"
          undefined
    _ -> undefined
