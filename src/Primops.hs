{-# LANGUAGE LambdaCase #-}
module Primops where

import LispkitParser (SExpr (..), toString)
import Data.Char

type BinOp = SExpr -> SExpr -> SExpr
type UnyOp = SExpr -> SExpr

binOp :: String -> Maybe BinOp
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
binOp _      = Nothing

binaryIntOp :: (Integer -> Integer -> Integer) -> BinOp
binaryIntOp op (SInt x) (SInt y) = SInt (x `op` y)

binOpCons :: BinOp
binOpCons hd (SList tl) = SList (hd:tl)
binOpCons hd tl         = SList [hd, tl]

unaryOp :: String -> Maybe UnyOp
unaryOp "car"  = Just opCar
unaryOp "cdr"  = Just opCdr
unaryOp "cadr" = Just $ opCar . opCdr
unaryOp "caar" = Just $ opCar . opCar
unaryOp "cdar" = Just $ opCdr . opCar
unaryOp "cddr" = Just $ opCdr . opCdr
unaryOp "sq"   = Just (\(SInt i) -> SInt (i * i))
unaryOp "odd"  = Just (\(SInt i) -> if rem i 2 /= 0 then SBool True else SBool False)
unaryOp "even" = Just (\(SInt i) -> if rem i 2 == 0 then SBool True else SBool False)
unaryOp "atom" = Just (\case
                         (SList _) -> SBool False
                         _         -> SBool True)
unaryOp "null" = Just (\case
                         (SList []) -> SBool True
                         _ -> SBool False)
unaryOp "not"  = Just (\(SBool x) -> SBool (not x))
unaryOp "chr"  = Just (\(SInt i)  -> SAtom [chr (fromInteger i)])
unaryOp "explode" = Just (\(SAtom atom) -> SList  $ map (\c -> SAtom [c]) atom)
unaryOp "implode" = Just (\(SList list) -> SAtom  $ concatMap toString list)
unaryOp _         = Nothing

opCar :: UnyOp
opCar (SList (hd:_)) = hd

opCdr :: UnyOp
opCdr (SList (_:tl)) = SList tl

isPrimOp :: String -> Bool
isPrimOp name = 
  case binOp name of
    Just _  -> True
    Nothing -> case unaryOp name of
      Just _  -> True
      Nothing -> False