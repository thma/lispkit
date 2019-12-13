{-# LANGUAGE FlexibleContexts #-}
module LambdaCompiler
    ( compileToLambda
    , compileEnv
    , parseTerm
    ) where

import Control.Monad.Except
import Data.Bifunctor
import LambdaTerm   (LTerm (..), LispkitError (..), BinOp, UnyOp)
import SExpr.LispkitParser (readSExpr, SExpr (..))
import LambdaPrimops

-- | parse a lambda term from a lisp symbolic expression
parseTerm :: (MonadError LispkitError m) => LTerm -> m LTerm
parseTerm (LVar v)  = return $ LVar v
parseTerm (LInt n)  = return $ LInt n
parseTerm (LBool b) = return $ LBool b
parseTerm (LList [LVar "lambda", LList vars, t]) = do
  t' <- parseTerm t
  return $ abstractVars vars t'
    where
      abstractVars [LVar var]      term = LAbs var term
      abstractVars (LVar var:rest) term = LAbs var (abstractVars rest term)

parseTerm (LList [LVar "quote", val]) =
  case val of
    l@(LList _) -> return (LUnyOp "quote" l)
    expr        -> do 
                     expr' <- parseTerm expr
                     return (LUnyOp "quote" expr')

parseTerm (LList [LVar fun, t1, t2]) = do
  t1' <- parseTerm t1
  t2' <- parseTerm t2
  case binOp fun of
    Just op -> return $ LBinPrimOp fun op t1' t2'
    Nothing -> return $ LBinOp fun t1' t2'

parseTerm (LList [LVar fun, t1]) = do
  t1' <- parseTerm t1
  case unaryOp fun of
    Just op -> return $ LUnyPrimOp fun op t1'
    Nothing -> return $ LUnyOp fun t1'

parseTerm (LList (t1:args)) = do
  t1' <- parseTerm t1
  args' <- mapM parseTerm args
  return $ LApp t1' args'

parseTerm term = throwError $ CompileError (show term)

-- | translate a Lisp Symbolic Expression to a LambdaTerm.
preTranslate :: (MonadError LispkitError m) => SExpr -> m LTerm
preTranslate (SAtom v)    = return $ LVar v
preTranslate (SInt n)     = return $ LInt n
preTranslate (SBool b)    = return $ LBool b
preTranslate (SList list) = do
  l <- mapM preTranslate list
  return $ LList l

-- | Compile the given lisp code to lambda terms
compileToLambda :: String -> Either LispkitError LTerm
compileToLambda = readSExpr' >=> preTranslate >=> parseTerm
  where
    readSExpr' :: String -> Either LispkitError SExpr
    readSExpr' = first (const ParseError) . readSExpr

compileEnv :: [(String, SExpr)] -> [(String, LTerm)]
compileEnv env =
  case compileEnvEither env of
    Right result -> result
    Left _       -> []
  where
    compileEnvEither :: [(String, SExpr)] -> Either LispkitError [(String, LTerm)]
    compileEnvEither env =
      let (keys, values) = unzip env
          lterms = mapM (preTranslate >=> parseTerm) values :: Either LispkitError [LTerm]
       in fmap (zip keys) lterms