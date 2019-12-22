{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module LambdaInterpreter where

import           LambdaTerm
import           LambdaPrimops
import           Control.Monad.Except
import           Data.Maybe    (fromMaybe)
import           LambdaCompiler (compile)

{-- Classic Eval/Apply interpreter on LTerms --}
 
-- | eval LTerm expression
eval :: (MonadError LispkitError m) => LTerm -> Environment -> m LTerm
eval num@(LInt _) _   = return num
eval bool@(LBool _) _ = return bool
eval exp@(LVar name) env = case lookup name env of
  Just val -> return val
  Nothing  -> if isPrimOp name
                then return exp
                else throwError (EvalError $ name ++ " not found")
eval (LUnyOp "eval" expr) env = do 
  expr' <- eval expr env
  eval expr' env
eval (LUnyOp "quote" expr) _ = return expr
eval lambda@(LAbs v body closure) _  = return lambda
eval (LApp (LVar "if") [test, thenPart, elsePart]) env = do 
  evalTest <- eval test env
  case evalTest of
    (LBool True)  -> eval thenPart env
    (LBool False) -> eval elsePart env
    _             -> throwError (EvalError $ "tertium non datur!" ++ show evalTest)

eval (LUnyPrimOp opName op arg) env = op <$> eval arg env
eval (LBinPrimOp opName op arg1 arg2) env = do
  arg1' <- eval arg1 env
  arg2' <- eval arg2 env
  return $ op arg1' arg2'
  
eval (LUnyOp opName arg) env =
  case lookup opName env of
    Just fun -> do 
      arg' <- eval arg env
      apply fun [arg'] env
    Nothing -> throwError (EvalError $ "undefined unary function " ++ opName)

eval (LBinOp opName arg1 arg2) env =
  case lookup opName env of
    Just fun -> do 
      arg1' <- eval arg1 env
      arg2' <- eval arg2 env
      apply fun [arg1', arg2'] env
    Nothing -> throwError (EvalError $ "undefined binary function " ++ opName)
    
eval (LApp fun@(LAbs var body closure) vals) env = apply fun vals env
    
eval (LApp fun args) env = do 
  fun' <- eval fun env
  apply fun' args env
  
eval list@(LList _) env = do
  term <- compile list
  eval term env


-- | apply function to arguments
apply :: (MonadError LispkitError m) => LTerm -> [LTerm] -> Environment -> m LTerm
apply (LVar fun) args env =
  case unaryOp fun of
    Just op -> eval (LUnyPrimOp fun op (head args)) env
    Nothing -> case binOp fun of
       Just op -> eval (LBinPrimOp fun op (head args) (head (tail args))) env
       Nothing -> case lookup fun env of
          Just op  -> eval (LApp op args) env
          Nothing  -> throwError (EvalError $ fun ++ " unknown function")

apply fun@(LAbs var body closure) vals env = eval innerBody (closure ++ env ++ localEnv )
  where
    vars      = getAbstractedVars fun
    localEnv  = zip vars vals
    innerBody = getInnermostBody body
    
    getAbstractedVars (LAbs var body closure) = var : getAbstractedVars body
    getAbstractedVars _ = []
    
    getInnermostBody (LAbs var body closure) = getInnermostBody body
    getInnermostBody body = body

apply fun@(LList _) args env = do
  fun' <- compile fun
  apply fun' args env

apply fun args _ = throwError (EvalError $ "Can't apply " ++ show fun ++ " to " ++ show args)