{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module CombinatorCompiler where

import LambdaTerm
import CombinatorTerm
import Control.Monad.Except


compile :: (MonadError LispkitError m) =>  LTerm -> m CombinatorTerm
compile (LInt int)   = return $ I (CInt int)
compile (LBool bool) = return $ I (CBool bool)
compile (LVar v)  = return $ CFree v
compile (LList l) = do
  list <- mapM compile l
  return $ CList list
  
compile (LBinPrimOp name op t1 t2) = do
  let cOp = k
  cT1 <- compile t1
  cT2 <- compile t2
  return (CBinOp name cOp cT1 cT2)

