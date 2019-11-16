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

evalString :: Environment -> String -> IO SExpr
evalString env expr = return $
  case readSExpr expr of
    Right x  -> eval x env
    Left err -> SAtom $ show err

evalFile :: Environment -> FilePath -> IO SExpr
evalFile env file = do
  putStrLn $ "loading " ++ file ++ "..."
  input <- readFile file
  evalString env input


repLoop :: Environment -> IO ()
repLoop env = do
  input <- readPrompt "> "
  case input of
    -- quit REPL
    ":q" -> do
      putStrLn "bye..."
      return ()
    -- load a file
    (':':'l':' ':file) -> do
      result <- evalFile env file
      putStrLn $ toString result
      repLoop $ ("_lastfile", SAtom file) : ("it", result) : env
    -- define a global value
    (':':'d':' ':nameVal) -> do
      let (name, value) = separateNameAndValue nameVal
      case readSExpr value of
        Right result -> do
          putStrLn $ "(define " ++ name ++ " " ++ toString result ++ ")"
          repLoop $ (name, result) : env
        Left err -> do
          putStrLn $ show err
          repLoop env
    -- reload last file
    ":r" ->
      case lookup "_lastfile" env of
        Just (SAtom file) -> do
          result <- evalFile env file
          putStrLn $ toString result
          repLoop $ ("it", result) : env
        Nothing -> do
          putStrLn "use :l to load a file first"
          repLoop env
    -- normal evaluation of lisp terms
    _ -> do
      result <- evalString env input
      putStrLn $ toString result
      repLoop $ ("it", result) : env :: IO ()

separateNameAndValue str = 
  let name  = (head . words) str
      value = drop (1 + length name) str
  in (name, value) 
      

main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  putStrLn "Welcome to lispkit"
  repLoop [("it", SAtom "welcome to lispkit")]


-- trapError action = catchError action (return . show)