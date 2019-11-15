module Main where

import LambdaCompiler
import LispkitParser
import LispkitInterpreter

import System.Environment
import Control.Monad
import Control.Monad.Except
import System.IO hiding(try)

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $
  case readSExpr expr of
    Right x  -> toString $ eval x []
    Left err -> show err

trapError action = catchError action (return . show)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
main = runRepl
