{-# LANGUAGE LambdaCase #-}
module LambdaPrimops where

import           LambdaTerm
import           Data.Char

type BinOp = LTerm -> LTerm -> LTerm
type UnyOp = LTerm -> LTerm

binOp :: String -> Maybe BinOp
binOp "+"    = Just $ binaryIntOp (+)
binOp "-"    = Just $ binaryIntOp (-)
binOp "*"    = Just $ binaryIntOp (*)
binOp "/"    = Just $ binaryIntOp div
binOp "%"    = Just $ binaryIntOp rem
binOp "eq"   = Just (\(LInt x) (LInt y) -> LBool (x == y))
binOp "leq"  = Just (\(LInt x) (LInt y) -> LBool (x <= y))
binOp "cons" = Just binOpCons
binOp "and"  = Just (\(LBool a) (LBool b) -> LBool (a && b))
binOp "or"   = Just (\(LBool a) (LBool b) -> LBool (a || b))
binOp _      = Nothing

binaryIntOp :: (Integer -> Integer -> Integer) -> BinOp
binaryIntOp op (LInt x) (LInt y) = LInt (x `op` y)

binOpCons :: BinOp
binOpCons hd (LList tl) = LList (hd:tl)
binOpCons hd tl         = LList [hd, tl]

unaryOp :: String -> Maybe UnyOp
unaryOp "car"  = Just opCar
unaryOp "cdr"  = Just opCdr
unaryOp "cadr" = Just $ opCar . opCdr
unaryOp "caar" = Just $ opCar . opCar
unaryOp "cdar" = Just $ opCdr . opCar
unaryOp "cddr" = Just $ opCdr . opCdr
unaryOp "sq"   = Just (\(LInt i) -> LInt (i * i))
unaryOp "odd"  = Just (\(LInt i) -> LBool (rem i 2 /= 0))
unaryOp "even" = Just (\(LInt i) -> LBool (rem i 2 == 0))
unaryOp "atom" = Just (\case
                         (LInt _)  -> LBool True
                         (LBool _) -> LBool True
                         (LVar _)  -> LBool True                         
                         _         -> LBool False)
unaryOp "null" = Just (\case
                         (LList []) -> LBool True
                         _          -> LBool False)
unaryOp "not"  = Just (\(LBool x) -> LBool (not x))
unaryOp "chr"  = Just (\(LInt i)  -> LVar [chr (fromInteger i)])
unaryOp "explode" = Just (\(LVar atom) -> LList $ map (\c -> LVar [c]) atom)
unaryOp _      = Nothing

opCar :: UnyOp
opCar (LList (hd:_)) = hd

opCdr :: UnyOp
opCdr (LList (_:tl)) = LList tl

isBinOp :: String -> Bool
isBinOp name = 
  case binOp name of
    Just _  -> True
    Nothing -> False
    
isUnaryOp :: String -> Bool
isUnaryOp name = 
  case unaryOp name of
    Just _  -> True
    Nothing -> False

isPrimOp :: String -> Bool
isPrimOp name = isUnaryOp name || isBinOp name
