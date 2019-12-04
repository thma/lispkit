{-# LANGUAGE FlexibleContexts #-}
module LambdaInterpreter where

import           LambdaCompiler
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
eval (LUnyPrimOp opName arg) env =
  case unaryOp opName of
    Just fun -> do
                  arg' <- eval arg env
                  return $ fun arg'
    Nothing  -> undefined -- can not occur apply (eval op env) (SList [eval x env]) env

eval term _ = throwError (EvalError $ "can't evaluate " ++ show term)
