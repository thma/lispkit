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

evalString :: Environment -> String -> IO String
evalString env expr = return $
  case readSExpr expr of
    Right x  -> toString $ eval x env
    Left err -> show err

trapError action = catchError action (return . show)

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env expr =
  case expr of
    ":r" -> undefined
    (':':'d':' ':name:' ':value) -> undefined
    (':':'l':' ':file) -> evalFile env file
    _    -> do
      result <- evalString env expr
      putStrLn result

evalFile :: Environment -> FilePath -> IO ()
evalFile env file = do
  putStrLn $ "loading " ++ file ++ "..."
  input <- readFile file
  result <- evalString env input
  putStrLn result

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: Environment -> IO ()
runRepl env = until_ (\x -> x == "quit" || x == ":q") (readPrompt "λ> ") (evalAndPrint env)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  runRepl [("hallo", SAtom "Welt")]
