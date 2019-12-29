{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module CombinatorCompiler where

import LambdaTerm
import CombinatorTerm
import Control.Monad.Except
--import Data.Bifunctor

compile :: (MonadError LispkitError m) =>  LTerm -> m CombinatorTerm
compile (LInt int)   = return $ I (CInt int)
compile (LBool bool) = return $ I (CBool bool)
compile (LVar v)  = return $ CFree v
compile (LList l) = do
  list <- mapM compile l
  return $ CList list

