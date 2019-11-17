{-# LANGUAGE FlexibleContexts #-}

module LambdaCompiler
    ( compile
    , parseTerm
    , CompileError(..)
    ) where

import Control.Monad.Except
import Data.Bifunctor
import LispkitParser
import Primops

data CompileError = CompileError
                  | ParseError
                    deriving Show

-- | 
data LTerm = LInt Integer
           | LVar String
           | LBinPrimOp String LTerm LTerm
           | LUnyPrimOp String LTerm LTerm
           | LApp LTerm LTerm
           | LAbs String LTerm
             deriving (Show, Eq)

-- | a lambda term from a lisp symbolic expression
parseTerm :: (MonadError CompileError m) => SExpr -> m LTerm
parseTerm (SAtom v) = return $ LVar v
parseTerm (SInt n) = return $ LInt n
parseTerm (SList [SAtom fun, t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  case binOp fun of
    Just op -> return $ LBinPrimOp fun t1' t2'
    Nothing  -> undefined --apply (eval op env) (SList [eval x env, eval y env]) env  
    
parseTerm (SList [SAtom "lambda", SList [SAtom var], t]) = do
  t' <- parseTerm t
  return $ LAbs var t'
parseTerm (SList [t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LApp t1' t2'
parseTerm _ = throwError CompileError



-- | Compile the given lisp code to lambda terms
compile :: String -> Either CompileError LTerm
compile = readSExpr' >=> parseTerm
  where
    readSExpr' :: String -> Either CompileError SExpr
    readSExpr' = first (const ParseError) . readSExpr

