module Value where

import Data.Char (toLower)
import Ast

data Value
  = Num Int
  | Str String
  | Bool Bool
  | Func String [Stmt]
  deriving (Show)

printValue :: Value -> IO ()
printValue v = do
  case v of
    Num n -> print n
    Str s -> putStrLn s
    Bool b ->
      let str = show b
          lowered = map toLower str
       in putStrLn lowered
    Func name body ->
      putStrLn $ "func " <> name

isTruthy :: Value -> Prelude.Bool
isTruthy v =
  case v of
    Bool b -> b
    Str s -> not $ null s
    Num n -> n /= 0
    Func _ _ -> True

addValues :: Value -> Value -> Value
addValues x y =
  case x of
    Num x' -> case y of
      Num y' -> Num (x' + y')
      _ -> undefined
    _ -> undefined

subValues :: Value -> Value -> Value
subValues x y =
  case x of
    Num x' -> case y of
      Num y' -> Num (x' - y')
      _ -> undefined
    _ -> undefined
