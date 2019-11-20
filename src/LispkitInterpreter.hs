module LispkitInterpreter where

import           Data.Char
import           Data.Maybe    (fromMaybe)
import           LispkitParser
import           Primops

type Environment = [(String, SExpr)]

eval :: SExpr -> Environment -> SExpr
eval num@(SInt _) _   = num
eval bool@(SBool _) _ = bool
eval exp@(SAtom name) env =
  case lookup name env of
    Just value -> value
    Nothing    -> if isPrimOp name then exp else error (name ++ " not found")
eval (SList [SAtom "quote", expr]) _   = expr
eval expr@(SList (SAtom "lambda":_)) _ = expr
eval (SList [SAtom "if", test, thenPart, elsePart]) env =
  case eval test env of
    (SBool True)  -> eval thenPart env
    (SBool False) -> eval elsePart env
eval (SList [SAtom "let", expr, definitions]) env =
  let localEnv = makeEnv definitions ++ env
   in eval expr localEnv
eval (SList [op@(SAtom opName), x, y]) env =
  case binOp opName of
    Just fun -> fun (eval x env) (eval y env)
    Nothing  -> apply (eval op env) (SList [eval x env, eval y env]) env
eval (SList [op@(SAtom opName), x]) env =
  case unaryOp opName of
    Just fun -> fun (eval x env)
    Nothing  -> apply (eval op env) (SList [eval x env]) env
eval (SList (fun@(SList [SAtom "lambda", SList vars, SList _body]):args)) env =
  apply fun (SList args) env
eval (SList (fun:args)) env = 
  eval (SList ((eval fun env):args)) env
eval x _ = error $ "No rule for evaluating " ++ toString x

apply :: SExpr -> SExpr -> Environment -> SExpr
apply fun@(SList [SAtom "lambda", SList vars, body@(SList _)]) (SList args) env = eval body localEnv
  where localEnv = zip (map (\(SAtom name) -> name) vars) args ++ env
apply fun args env = error $ "apply issue: " ++ "\n fun: " ++ toString fun ++ "\nargs: " ++ toString args

makeEnv :: SExpr -> Environment
makeEnv (SList [])                  = []
makeEnv (SList (SAtom name:val:tl)) = (name, val) : makeEnv (SList tl)
