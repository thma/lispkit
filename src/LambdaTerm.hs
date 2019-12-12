{-# LANGUAGE DeriveGeneric #-}
module LambdaTerm 
  ( LTerm (..)
  , CompileError (..)
  , toString
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
             deriving (Generic, Eq)

instance Show LTerm where
  show (LInt i) = "LInt " ++ show i
  show (LBool b) = "LBool " ++ show b
  show (LVar v) = "LVar " ++ show v
  show (LList list) = "LList " ++ show list
  show (LBinPrimOp op t1 t2) = "LBinPrimOp " ++ op ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LBinOp op t1 t2) = "LBinOp " ++ op ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LUnyPrimOp op t1) = "LUnyPrimOp " ++ op ++ " (" ++ show t1 ++ ")"
  show (LUnyOp op t1) = "LUnyOp " ++ op ++ " (" ++ show t1 ++ ")"
  show (LApp t1 t2) = "LApp (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (LAbs var term) = "LAbs " ++ var ++ " (" ++ show term ++ ")"

data CompileError = CompileError String
                  | EvalError String
                  | ParseError
                    deriving (Show)


toString :: LTerm -> String
toString (LVar str) = str
toString (LInt i)    = show i
toString (LBool bool) =  if bool then "true" else "false"
toString (LBinPrimOp op t1 t2) = "(" ++ op ++ " " ++ toString t1 ++ " " ++ toString t2 ++ ")"
toString (LBinOp op t1 t2) = "(" ++ op ++ " " ++ toString t1 ++ " " ++ toString t2 ++ ")"
toString (LUnyPrimOp op t1) = "(" ++ op ++ " " ++ toString t1 ++ ")"
toString (LUnyOp op t1) = "(" ++ op ++ " " ++ toString t1 ++ ")"
toString (LAbs var term) = "(lambda (" ++ var ++ ") " ++ toString term ++ ")"
toString (LApp fun terms) = "(" ++ toString fun ++ " " ++ render terms ++ ")"
toString (LList list) = "(" ++ render list ++ ")"


render [] = ""
render [hd] = toString hd
render (hd:tl) = toString hd ++ " " ++ render tl