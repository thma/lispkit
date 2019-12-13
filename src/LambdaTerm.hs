{-# LANGUAGE DeriveGeneric #-}
module LambdaTerm 
  ( LTerm (..)
  , LispkitError (..)
  , BinOp
  , UnyOp
  , toString
  ) where
  
import GHC.Generics

-- | a lambda term representation with support for ints, lists, and unary and binary primitive operations
data LTerm = LInt Integer
           | LBool Bool
           | LVar String
           | LList [LTerm]
           | LBinPrimOp String BinOp LTerm LTerm
           | LBinOp String LTerm LTerm
           | LUnyPrimOp String UnyOp LTerm
           | LUnyOp String LTerm
           | LApp LTerm [LTerm]
           | LAbs String LTerm
             deriving (Generic)
             
instance Eq LTerm where
  (LInt i)  == (LInt i')                                = i == i'              
  (LBool b) == (LBool b')                               = b == b'
  (LVar v)  == (LVar v')                                = v == v'
  (LList l) == (LList l')                               = l == l'
  (LBinPrimOp op _ t1 t2) == (LBinPrimOp op' _ t1' t2') = op == op' && t1 == t1' && t2 == t2'
  (LBinOp op t1 t2)       == (LBinOp op' t1' t2')       = op == op' && t1 == t1' && t2 == t2'       
  (LUnyPrimOp op _ t1)    == (LUnyPrimOp op' _ t1')     = op == op' && t1 == t1'  
  (LUnyOp op t1)          == (LUnyOp op' t1')           = op == op' && t1 == t1'         
  (LApp t1 t2)    == (LApp t1' t2')                     = t1 == t1' && t2 == t2' 
  (LAbs var term) == (LAbs var' term')                  = var == var' && term == term'         
  x == y = False
  
  
  
instance Show LTerm where
  show (LInt i) = "LInt " ++ show i
  show (LBool b) = "LBool " ++ show b
  show (LVar v) = "LVar " ++ show v
  show (LList list) = "LList " ++ show list
  show (LBinPrimOp op _ t1 t2) = "LBinPrimOp " ++ op ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LBinOp op t1 t2) = "LBinOp " ++ op ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LUnyPrimOp op _ t1) = "LUnyPrimOp " ++ op ++ " (" ++ show t1 ++ ")"
  show (LUnyOp op t1) = "LUnyOp " ++ op ++ " (" ++ show t1 ++ ")"
  show (LApp t1 t2) = "LApp (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LAbs var term) = "LAbs " ++ var ++ " (" ++ show term ++ ")"

data LispkitError = CompileError String
                  | EvalError String
                  | ParseError
                    deriving (Show)

type BinOp = LTerm -> LTerm -> LTerm
type UnyOp = LTerm -> LTerm

toString :: LTerm -> String
toString (LVar str)  = str
toString (LInt i)    = show i
toString (LBool bool) = if bool then "true" else "false"
toString (LBinPrimOp op _ t1 t2) = "(" ++ op ++ " " ++ toString t1 ++ " " ++ toString t2 ++ ")"
toString (LBinOp op t1 t2)       = "(" ++ op ++ " " ++ toString t1 ++ " " ++ toString t2 ++ ")"
toString (LUnyPrimOp op _ t1) = "(" ++ op ++ " " ++ toString t1 ++ ")"
toString (LUnyOp op t1)       = "(" ++ op ++ " " ++ toString t1 ++ ")"
toString (LAbs var term) = "(lambda (" ++ var ++ ") " ++ toString term ++ ")"
toString (LApp fun terms) = "(" ++ toString fun ++ " " ++ render terms ++ ")"
toString (LList list) = "(" ++ render list ++ ")"


render [] = ""
render [hd] = toString hd
render (hd:tl) = toString hd ++ " " ++ render tl