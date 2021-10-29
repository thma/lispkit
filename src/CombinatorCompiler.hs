{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module CombinatorCompiler where

import LambdaTerm
import CombinatorTerm
import Control.Monad.Except

{--
data LTerm = LInt Integer
           | LBool Bool
           | LVar String
           | LList [LTerm]
           | LBinPrimOp String BinOp LTerm LTerm
           | LBinOp String LTerm LTerm
           | LUnyPrimOp String UnyOp LTerm
           | LUnyOp String LTerm
           | LApp LTerm [LTerm]
           | LAbs String LTerm Environment
--}

compile :: (MonadError LispkitError m) =>  LTerm -> m CombinatorTerm
compile (LInt int)   = return $ I (CInt int)
compile (LBool bool) = return $ I (CBool bool)
compile (LVar v)  = return $ CFree v
compile (LList l) = do
  list <- mapM compile l
  return $ CList list
  
compile (LBinPrimOp name op t1 t2) = do
  let cOp = k -- TODO !!
  cT1 <- compile t1
  cT2 <- compile t2
  return (CBinOp name cOp cT1 cT2)
  
compile (LBinOp name t1 t2) = do
  let cOp = k -- TODO !!
  cT1 <- compile t1
  cT2 <- compile t2
  return (CBinOp name cOp cT1 cT2)

compile (LUnyPrimOp name op t1) = do
  let cOp = i -- TODO !!
  cT1 <- compile t1
  return (CUnyOp name cOp cT1)

compile (LUnyOp name t1) = do
  let cOp = i -- TODO !!
  cT1 <- compile t1
  return (CUnyOp name cOp cT1)  
  
compile (LApp t1 t2) = do
  cT1  <- compile t1
  cT2s <- mapM compile t2  
  return (CApp cT1 (toAppTree cT2s)) 
  where
    toAppTree :: [CombinatorTerm] -> CombinatorTerm
    toAppTree = foldr CApp CNil

compile (LAbs var term env) = do
  return CNil -- TODO !!
  
--bracket :: String -> LTerm -> CombinatorTerm
--bracket var (LVar x) = case var == v of
--  true  -> I
--  false -> CFree x
--bracket var 