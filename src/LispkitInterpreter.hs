module LispkitInterpreter where

import LispkitParser
import Primops
import Data.Maybe (fromMaybe)
import Data.Char


type Environment = [(String, SExpr)]


makeEnv :: SExpr -> Environment
makeEnv (SList []) = []
makeEnv (SList (SAtom name:val:tl)) = (name, val) : makeEnv (SList tl)


eval :: SExpr -> Environment -> SExpr
eval num@(SInt _) _   = num
eval bool@(SBool _) _ = bool
eval (SAtom name) env = fromMaybe (error (name ++ " not found")) (lookup name env)
eval (SList [SAtom "quote", x]) _ = x
eval (SList [SAtom "lambda", x]) _ = x
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

eval x _ = error $ "No rule for evaluating " ++ toString x

apply :: SExpr -> SExpr -> Environment -> SExpr
apply fun@(SList [SAtom "lambda", SList vars, body@(SList _)]) (SList args) env = eval body localEnv
  where localEnv = zip (map (\(SAtom name) -> name) vars) args ++ env
apply fun args env = error $ "apply issue: " ++ "\n fun: " ++ toString fun ++ "\nargs: " ++ toString args -- ++ "\n env: " ++ show env






