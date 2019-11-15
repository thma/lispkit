module LispkitInterpreter where

import LispkitParser
import Data.Maybe (fromMaybe)

type Environment = [(String, SExpr)]

eval :: SExpr -> Environment -> SExpr
eval num@(SInt i) _   = num
eval (SAtom name) env = fromMaybe (SError (name ++ " not found")) (lookup name env)
eval (SList [SAtom "quote", x]) _ = x
eval (SList [SAtom "lambda", x]) _ = x
eval (SList [SAtom "if", test, thenPart, elsePart]) env =
  case (eval test env) of
    (SInt 0) -> eval elsePart env
    (SInt 1) -> eval thenPart env

eval (SList [SAtom "let", expr, definitions]) env =
  let localEnv = env
   in eval expr localEnv

eval (SList [op@(SAtom opName), x, y]) env =
  case binOp opName of
    Just fun -> fun (eval x env) (eval y env)
    Nothing  -> apply (eval op env) (SList [eval x env, eval y env]) env
eval (SList [op@(SAtom opName), x]) env =
  case unaryOp opName of
    Just fun -> fun (eval x env)
    Nothing  -> apply (eval op env) (eval x env) env

eval (SList (fun@(SList [SAtom "lambda", SList vars, SList body]):args)) env =
  apply fun (SList args) env

eval x _ = SError $ "No rule for evaluating " ++ show x

apply :: SExpr -> SExpr -> Environment -> SExpr
apply fun@(SList [SAtom "lambda", SList vars, body@(SList _)]) (SList args) env = eval body localEnv
  where localEnv = zip (map (\(SAtom name) -> name) vars) args ++ env

binOp :: String -> Maybe (SExpr -> SExpr -> SExpr)
binOp "+" = Just $ binaryIntOp (+)
binOp "-" = Just $ binaryIntOp (-)
binOp "*" = Just $ binaryIntOp (*)
binOp "/" = Just $ binaryIntOp div
binOp "%" = Just $ binaryIntOp rem
binOp "eq" = Just (\(SInt x) (SInt y) -> if x == y then SInt 1 else SInt 0)
binOp "cons" = Just binOpCons
binOp _   = Nothing

binaryIntOp :: (Integer -> Integer -> Integer) -> SExpr -> SExpr -> SExpr
binaryIntOp op (SInt x) (SInt y) = SInt (x `op` y)

binOpCons :: SExpr -> SExpr -> SExpr
binOpCons hd (SList tl) = SList (hd:tl)
binOpCons hd tl         = SList [hd, tl]

unaryOp :: String -> Maybe (SExpr -> SExpr)
unaryOp "car"  = Just opCar
unaryOp "cdr"  = Just opCdr
unaryOp "cadr" = Just $ opCar . opCdr
unaryOp "caar" = Just $ opCar . opCar
unaryOp "cdar" = Just $ opCdr . opCar
unaryOp "cddr" = Just $ opCdr . opCdr
unaryOp _      = Nothing

opCar :: SExpr -> SExpr
opCar (SList (hd:_)) = hd

opCdr :: SExpr -> SExpr
opCdr (SList (_:tl)) = SList tl



