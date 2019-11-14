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
  case binOp opName of
    Just fun -> fun (eval x env) (eval y env)
    Nothing  -> apply (eval op env) (eval x env) (eval y env)

eval (SList [op@(SAtom opName), x]) env
  | isUnaryOp op = unaryOp opName (eval x env)
  | otherwise    = apply (eval op env) (eval x env)

eval x _ = SError $ "No rule for evaluating " ++ show x

apply = undefined

binOp :: String -> Maybe (SExpr -> SExpr -> SExpr)
binOp "+" = Just $ binaryIntOp (+)
binOp "-" = Just $ binaryIntOp (-)
binOp "*" = Just $ binaryIntOp (*)
binOp "/" = Just $ binaryIntOp div
binOp "%" = Just $ binaryIntOp rem
binOp "cons" = Just binOpCons
binOp _   = Nothing

binaryIntOp :: (Integer -> Integer -> Integer) -> SExpr -> SExpr -> SExpr
binaryIntOp op (SInt x) (SInt y) = SInt (x `op` y)

binOpCons :: SExpr -> SExpr -> SExpr
binOpCons hd (SList tl) = SList (hd:tl)
binOpCons hd tl         = SList [hd, tl]


isUnaryOp :: SExpr -> Bool
isUnaryOp (SAtom op) = op `elem` ["car", "cdr", "caar", "cadr", "cdar", "cddr"]

unaryOp :: String -> SExpr -> SExpr
unaryOp "car" = opCar
unaryOp "cdr" = opCdr
unaryOp "cadr" = opCar . opCdr
unaryOp "caar" = opCar . opCar
unaryOp "cdar" = opCdr . opCar
unaryOp "cddr" = opCdr . opCdr

opCar :: SExpr -> SExpr
opCar (SList (hd:_)) = hd

opCdr :: SExpr -> SExpr
opCdr (SList (_:tl)) = SList tl




