module Main where

import System.Environment
import Control.Monad
import Control.Monad.Except
--import System.IO hiding(try)
import           System.IO                (hSetEncoding, stdin, stdout, utf8, hFlush)

import LispkitParser
import LispkitInterpreter

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
evalAndPrint expr =
  case expr of
    ":r" -> undefined
    (':':'l':' ':file) -> evalFile file
    _    -> do
      result <- evalString expr
      putStrLn result

evalFile :: FilePath -> IO ()
evalFile file = do
  putStrLn $ "loading " ++ file ++ "..."
  input <- readFile file
  result <- evalString input
  putStrLn result

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (\x -> x == "quit" || x == ":q") (readPrompt "λ> ") evalAndPrint

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  runRepl
