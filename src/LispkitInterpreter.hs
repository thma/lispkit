module LispkitInterpreter where

import LispkitParser
import Data.Maybe (fromMaybe)

type Environment = [(String, SExpr)]

eval :: SExpr -> Environment -> SExpr
eval num@(SInt i) _   = num
eval (SAtom name) env = fromMaybe (SError (name ++ " not found")) (lookup name env)
eval (SList [SAtom "quote", x]) _ = x
eval (SList [SAtom "lambda", x]) _ = x

eval (SList [op@(SAtom opName), x, y]) env =
  if isBinOp op 
    then binOp opName (eval x env) (eval y env) 
    else apply (eval op env) (eval x env) (eval y env)


eval (SList [op@(SAtom opName), x]) env = if isUnaryOp op then unaryOp opName (eval x env) else SError "nöp"

eval x _ = SError $ "No rule for evaluating " ++ show x

isBinOp :: SExpr -> Bool
isBinOp (SAtom op) = op `elem` ["+","-","*","/","%","cons"]

binOp :: String -> SExpr -> SExpr -> SExpr
binOp "+" = binOpAdd

binOpAdd :: SExpr -> SExpr -> SExpr
binOpAdd (SInt x) (SInt y) = SInt (x + y)

isUnaryOp :: SExpr -> Bool
isUnaryOp (SAtom op) = op `elem` ["car", "cdr"]

unaryOp :: String -> SExpr -> SExpr
unaryOp "car" = opCar

opCar :: SExpr -> SExpr
opCar (SList (hd:tl)) = hd

apply = undefined


