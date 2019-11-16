{-# LANGUAGE LambdaCase #-}
module LispkitInterpreter where

import LispkitParser
import Data.Maybe (fromMaybe)
import Data.Char

type Environment = [(String, SExpr)]


makeEnv :: SExpr -> Environment
makeEnv (SList []) = []
makeEnv (SList (SAtom name:val:tl)) = (name, val) : makeEnv (SList tl)


eval :: SExpr -> Environment -> SExpr
eval num@(SInt _) _   = num
eval bool@(SBool _) _ = bool
eval (SAtom name) env = fromMaybe (SError (name ++ " not found")) (lookup name env)
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

eval x _ = SError $ "No rule for evaluating " ++ show x

apply :: SExpr -> SExpr -> Environment -> SExpr
apply fun@(SList [SAtom "lambda", SList vars, body@(SList _)]) (SList args) env = eval body localEnv
  where localEnv = zip (map (\(SAtom name) -> name) vars) args ++ env
apply fun args env = SError ("apply issue: " ++ "\n fun: " ++ show fun ++ "\nargs: " ++ show args ++ "\n env: " ++ show env)


binOp :: String -> Maybe (SExpr -> SExpr -> SExpr)
binOp "+"    = Just $ binaryIntOp (+)
binOp "-"    = Just $ binaryIntOp (-)
binOp "*"    = Just $ binaryIntOp (*)
binOp "/"    = Just $ binaryIntOp div
binOp "%"    = Just $ binaryIntOp rem
binOp "eq"   = Just (\(SInt x) (SInt y) -> if x == y then SBool True else SBool False)
binOp "leq"  = Just (\(SInt x) (SInt y) -> if x <= y then SBool True else SBool False)
binOp "cons" = Just binOpCons
binOp "and"  = Just (\(SBool a) (SBool b) -> if a && b then SBool True else SBool False)
binOp "or"   = Just (\(SBool a) (SBool b) -> if a || b then SBool True else SBool False)
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
unaryOp "sq"   = Just (\(SInt i) -> SInt (i * i))
unaryOp "odd"  = Just (\(SInt i) -> if rem i 2 == 0 then SBool True else SBool False)
unaryOp "even" = Just (\(SInt i) -> if rem i 2 /= 0 then SBool True else SBool False)
unaryOp "atom" = Just (\case
                         (SList _) -> SBool False
                         _         -> SBool True)
unaryOp "null" = Just (\case
                         (SList []) -> SBool True
                         _ -> SBool False)
unaryOp "not"  = Just (\(SBool x) -> SBool (not x))
unaryOp "chr"  = Just (\(SInt i)  -> SAtom [chr (fromInteger i)])

unaryOp _      = Nothing

opCar :: SExpr -> SExpr
opCar (SList (hd:_)) = hd

opCdr :: SExpr -> SExpr
opCdr (SList (_:tl)) = SList tl

toString :: SExpr -> String
toString (SAtom str) = str
toString (SInt i)    = show i
toString (SBool bool) =  if bool then "true" else "false"
toString (SError str) = str
toString (SList list) = "(" ++ render list ++ ")"
  where
    render [] = ""
    render [hd] = toString hd
    render (hd:tl) = toString hd ++ " " ++ render tl




