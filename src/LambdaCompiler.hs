{-# LANGUAGE FlexibleContexts #-}
module LambdaCompiler
    ( compileToLambda
    , compileEnv
    , parseTerm
    , CompileError(..)
    , LTerm (..)
    ) where

import Control.Monad.Except
import Data.Bifunctor
import LispkitParser
import Primops

data CompileError = CompileError String
                  | ParseError
                    deriving Show

-- | 
data LTerm = LInt Integer
           | LBool Bool
           | LVar String
           | LBinPrimOp String LTerm LTerm
           | LBinOp String LTerm LTerm
           | LUnyPrimOp String LTerm
           | LUnyOp String LTerm
           | LApp LTerm [LTerm]
           | LAbs String LTerm
             deriving (Show, Eq)

-- | parse a lambda term from a lisp symbolic expression
parseTerm :: (MonadError CompileError m) => SExpr -> m LTerm
parseTerm (SAtom v) = return $ LVar v
parseTerm (SInt n)  = return $ LInt n
parseTerm (SBool b) = return $ LBool b
parseTerm (SList [SAtom "lambda", SList vars, t]) = do
  t' <- parseTerm t
  return $ abstractVars vars t'
    where
      abstractVars [SAtom var]        term = LAbs var term
      abstractVars (SAtom var : rest) term = LAbs var (abstractVars rest term)

parseTerm (SList [SAtom fun, t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  case binOp fun of
    Just op -> return $ LBinPrimOp fun t1' t2'
    Nothing -> return $ LBinOp fun t1' t2'

parseTerm (SList [SAtom fun, t1]) = do
  t1' <- parseTerm t1
  case unaryOp fun of
    Just op -> return $ LUnyPrimOp fun t1'
    Nothing -> return $ LUnyOp fun t1'

parseTerm (SList (t1:args)) = do
  t1' <- parseTerm t1
  args' <- mapM parseTerm args
  return $ LApp t1' args'

parseTerm term = throwError $ CompileError (show term)


-- | Compile the given lisp code to lambda terms
compileToLambda :: String -> Either CompileError LTerm
compileToLambda = readSExpr' >=> parseTerm
  where
    readSExpr' :: String -> Either CompileError SExpr
    readSExpr' = first (const ParseError) . readSExpr

compileEnvEither :: [(String, SExpr)] -> Either CompileError [(String, LTerm)]
compileEnvEither env =
  let (keys, values) = unzip env
      lterms = mapM parseTerm values :: Either CompileError [LTerm]
   in fmap (zip keys) lterms

compileEnv :: [(String, SExpr)] -> [(String, LTerm)]
compileEnv env =
  case compileEnvEither env of
    Right result -> result
    Left _       -> []

test :: IO ()
test = do
  let expr = "(lambda (n) (+ (* n 8) m))"
  case compileToLambda expr of
    Right term -> print term
    Left  err  -> print err