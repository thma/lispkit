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
             deriving (Generic, Show, Eq)

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