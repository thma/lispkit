{-# LANGUAGE FlexibleContexts #-}
module LambdaInterpreter where

import           LambdaTerm
import           LambdaPrimops
import           Control.Monad.Except
import           Data.Maybe    (fromMaybe)

type Environment = [(String, LTerm)]

newtype EvalError = EvalError String deriving Show

type EvalErrorMonad = Either EvalError

--eval :: (MonadError EvalError m) => LTerm -> Environment -> m LTerm
eval :: LTerm -> Environment -> EvalErrorMonad LTerm
eval num@(LInt _) _   = return num
eval bool@(LBool _) _ = return bool
eval exp@(LVar name) env = case lookup name env of
  Just val -> return val
  Nothing  -> if isPrimOp name
                then return exp
                else throwError (EvalError $ name ++ " not found")
eval (LUnyOp "quote" expr) _ = return expr
eval lambda@(LAbs v body) _  = return lambda
eval (LApp (LVar "if") [test, thenPart, elsePart]) env = do 
  evalTest <- eval test env
  case evalTest of
    (LBool True)  -> eval thenPart env
    (LBool False) -> eval elsePart env
eval (LUnyPrimOp opName arg) env = do
  fun <- unyOp opName
  fun <$> eval arg env
eval (LBinPrimOp opName arg1 arg2) env = do
  fun   <- biOp opName
  arg1' <- eval arg1 env
  arg2' <- eval arg2 env
  return $ fun arg1' arg2'

eval (LApp fun@(LAbs var body) vals) env = eval innerBody localEnv
  where
    vars      = getAbstractedVars fun
    localEnv  = zip vars vals ++ env
    innerBody = getInnermostBody body
    
    getAbstractedVars (LAbs var body) = var : getAbstractedVars body
    getAbstractedVars _ = []
    
    getInnermostBody (LAbs var body) = getInnermostBody body
    getInnermostBody body = body
    
eval (LApp fun args) env = do 
  fun' <- eval fun env
  apply fun' args env

eval term _ = throwError (EvalError $ "can't evaluate " ++ show term)


apply (LVar fun) args env =
  case unaryOp fun of
    Just op -> eval (LUnyPrimOp fun (head args)) env
    Nothing -> case binOp fun of
       Just op -> eval (LBinPrimOp fun (head args) (head (tail args))) env
       Nothing -> case lookup fun env of
          Just op -> eval (LApp op args) env
          Nothing  -> throwError (EvalError $ fun ++ " unknown function")

unyOp :: MonadError EvalError m => String -> m UnyOp
unyOp name =
  case unaryOp name of
    Just fun -> return fun
    Nothing  -> throwError (EvalError $ name ++ " is not a unary primitive operation")

biOp :: MonadError EvalError m => String -> m BinOp
biOp name =
  case binOp name of
    Just fun -> return fun
    Nothing  -> throwError (EvalError $ name ++ " is not a binary primitive operation")