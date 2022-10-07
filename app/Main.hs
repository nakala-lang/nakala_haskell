module Main where

import Ast (Stmt)
import GHC.Base (MonadPlus (mzero))
import Interpreter (eval)
import qualified Parser (parseProgram)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "\nnakala (haskell edition)\n"
  repl

repl :: IO ()
repl = do
  n <- putStr "> "
  hFlush stdout
  input <- getLine :: IO String
  case input of
    ".exit" -> return ()
    _ -> do
      putStrLn input
      hFlush stdout
      case Parser.parseProgram input of
        Left err -> print err
        Right ast -> do
          print ast
          eval ast
      repl
