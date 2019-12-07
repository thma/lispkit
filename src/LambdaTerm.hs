{-# LANGUAGE DeriveGeneric #-}
module LambdaTerm 
  ( LTerm (..)
  ) where
  
import GHC.Generics

-- | a lambda term representation with support for ints, lists, and unary and binary primitive operations
data LTerm = LInt Integer
           | LBool Bool
           | LVar String
           | LList [LTerm]
           | LBinPrimOp String LTerm LTerm
           | LBinOp String LTerm LTerm
           | LUnyPrimOp String LTerm
           | LUnyOp String LTerm
           | LApp LTerm [LTerm]
           | LAbs String LTerm
             deriving (Generic, Show, Eq)