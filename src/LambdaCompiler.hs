{-# LANGUAGE FlexibleContexts #-}

module LambdaCompiler
    ( compile
    , CompileError(..)
    ) where

import Control.Monad.Except
import Data.Bifunctor
import LispkitParser

data CompileError = CompileError
                  | ParseError
                    deriving Show

-- | 
data LTerm = LInt Integer
           | LVar String
           | LAdd LTerm LTerm
           | LSub LTerm LTerm
           | LMul LTerm LTerm
           | LDiv LTerm LTerm
           | LMod LTerm LTerm
           | LApp LTerm LTerm
           | LAbs String LTerm
             deriving (Show, Eq)

-- | a lambda term from a lisp symbolic expression
parseTerm :: (MonadError CompileError m) => SExpr -> m LTerm
parseTerm (SAtom v) = return $ LVar v
parseTerm (SInt n) = return $ LInt n
parseTerm (SList [SAtom "+", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LAdd t1' t2'
parseTerm (SList [SAtom "-", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LSub t1' t2'
parseTerm (SList [SAtom "*", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LMul t1' t2'
parseTerm (SList [SAtom "/", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LDiv t1' t2'
parseTerm (SList [SAtom "%", t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  return $ LMod t1' t2'
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

