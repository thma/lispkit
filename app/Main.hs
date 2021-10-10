module Main where

import           Control.Monad
import           Control.Monad.Except
import           System.Environment
import           System.IO            (hFlush, hSetEncoding, stdin, stdout, utf8)
import           LambdaCompiler       (compileToLambda, preCompileToLambda)
import           LambdaInterpreter
import           LambdaTerm
import           CombinatorCompiler   (compile)

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalFile :: Environment -> FilePath -> IO LTerm
evalFile env file = do
  putStrLn $ "loading " ++ file ++ "..."
  input <- readFile file
  evalString env input

evalString :: Environment -> String -> IO LTerm
evalString env input = return $
  case compileToLambda input of
   Right term -> case eval term env of
     Right result -> result
     Left err       -> LVar $ show err
   Left  err      -> LVar $ show err

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
      repLoop $ ("_lastfile", LVar file) : ("it", result) : env
    -- define a global value
    ('(':'d':'e':'f':'i':'n':'e':' ':nameVal)
     -> do
      let (name, value) = separateNameAndValue nameVal
      result <- evalString env value
      putStrLn $ "(define " ++ name ++ " " ++ toString result ++ ")"
      repLoop $ (name, result) : env
    -- reload last file
    ":r" ->
      case lookup "_lastfile" env of
        Just (LVar file) -> do
          result <- evalFile env file
          putStrLn $ toString result
          repLoop $ ("it", result) : env
        Just x -> do
          putStrLn $ "Error: " ++ show x ++ " is not a file"
          repLoop env
        Nothing -> do
          putStrLn "use :l to load a file first"
          repLoop env
    -- normal evaluation of lisp terms
    _ -> do
      case preCompileToLambda input of
        Right term -> print term
        Left  err  -> print err
      
      case compileToLambda input of
        Right term -> do
          print term
          
          -- also do compilation to SKI  
          case compile term of
            Right cTerm -> print cTerm
            Left  err   -> print err
        
          let result = eval term env
          print result
          case result of
            Right term -> do
              putStrLn $ toString term
              repLoop $ ("it", term) : env -- :: IO ()
            Left err -> print err
          

        Left err -> print err


separateNameAndValue :: [Char] -> (String, [Char])
separateNameAndValue str =
  let name  = (head . words) str
      value = drop (1 + length name) str
   in (name, value)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  putStrLn "Welcome to lispkit"
  repLoop []