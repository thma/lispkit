{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module LambdaCompiler
    ( compileToLambda
    , compile
    , preCompileToLambda
    ) where

import Control.Monad.Except
import Data.Bifunctor
import LambdaTerm   (LTerm (..), LispkitError (..), BinOp, UnyOp)
import SExpr.LispkitParser (readSExpr, SExpr (..))
import LambdaPrimops

-- | parse a lambda term from a lisp symbolic expression
compile :: (MonadError LispkitError m) => LTerm -> m LTerm
compile (LVar v)  = return $ LVar v
compile (LInt n)  = return $ LInt n
compile (LBool b) = return $ LBool b
compile (LList [LVar "lambda", LList vars, t]) = do
  t' <- compile t
  return $ abstractVars vars t'
    where
      abstractVars [LVar var]      term = LAbs var term []
      abstractVars (LVar var:rest) term = LAbs var (abstractVars rest term) []
      abstractVars vars term = error $ "malformed lambda: " ++ show vars ++ " " ++ show term

compile (LList [LVar "quote", val]) =
  case val of
    l@(LList _) -> return (LUnyOp "quote" l)
    expr        -> do 
                     expr' <- compile expr
                     return (LUnyOp "quote" expr')

compile (LList (LVar "let" : body : definitions)) = do
  let vars = getVars definitions
      vals = getVals definitions      
  vals' <- mapM compile vals
  body' <- compile body
  return $ createApp vars vals' body'
  where
    getVars :: [LTerm] -> [String]
    getVars [] = []
    getVars (LList [LVar var, _] : rest) = var : getVars rest
    getVars _ = error $ "malformed let: vars: " ++ show definitions
    
    getVals :: [LTerm] -> [LTerm]
    getVals [] = []
    getVals (LList [_, val] : rest) = val : getVals rest
    getVals _ = error $ "malformed let: vals: " ++ show definitions
    
    createApp :: [String] -> [LTerm] -> LTerm -> LTerm
    createApp [] [] body = body
    createApp (var:vars) (val:vals) body = LApp (LAbs var (createApp vars vals body) (zip vars vals)) [val]
    createApp vars vals _ = error $ "malformed let: vars and vals: " ++ show vars ++ " " ++ show vals


{--
;; letrec macro, example:
;;(letrec
;;	((even? (lambda (n) (if (zero? n) true (odd? (sub1 n)))))
;;	 (odd? (lambda (n) (if (zero? n) false (even? (sub1 n))))))
;;	(odd? 37))
;; macro expands to ==>
;;(let 
;;  ((even? (lambda (n even? odd?) (if (zero? n) true (odd? (sub1 n) even? odd?))))
;;   (odd? (lambda (n even? odd?) (if (zero? n) false (even? (sub1 n) even? odd?)))))
;;  (odd? 37 even? odd?))
;; and thus finally to ==>
;;((lambda (even? odd?) (odd? 37 even? odd?)) 
;;	(lambda (n even? odd?) (if (zero? n) true (odd? (sub1 n) even? odd?)))
;;	(lambda (n even? odd?) (if (zero? n) false (even? (sub1 n) even? odd?))))   
--}

compile (LList [LVar fun, t1, t2]) = do
  t1' <- compile t1
  t2' <- compile t2
  case binOp fun of
    Just op -> return $ LBinPrimOp fun op t1' t2'
    Nothing -> return $ LBinOp fun t1' t2'

compile (LList [LVar fun, t1]) = do
  t1' <- compile t1
  case unaryOp fun of
    Just op -> return $ LUnyPrimOp fun op t1'
    Nothing -> return $ LUnyOp fun t1'

compile (LList (t1:args)) = do
  t1' <- compile t1
  args' <- mapM compile args
  return $ LApp t1' args'

compile term = throwError $ CompileError (show term)

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
compileToLambda = readSExpr' >=> preTranslate >=> compile
  where
    readSExpr' :: String -> Either LispkitError SExpr
    readSExpr' = first (const ParseError) . readSExpr

-- | only useful for debugging
preCompileToLambda :: String -> Either LispkitError LTerm
preCompileToLambda = readSExpr' >=> preTranslate
  where
    readSExpr' :: String -> Either LispkitError SExpr
    readSExpr' = first (const ParseError) . readSExpr
