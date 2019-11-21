{-# LANGUAGE FlexibleContexts #-}
module LambdaInterpreter where

import           LambdaCompiler
import           Primops
import           Control.Monad.Except

type Environment = [(String, LTerm)]

newtype EvalError = EvalError String deriving Show

--eval :: (MonadError EvalError m) => LTerm -> Environment -> m LTerm
eval num@(LInt _) _   = return num
eval bool@(LBool _) _ = return bool
