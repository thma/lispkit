module Lambda.LambdaInterpreterSpec where

import           Control.Exception     (evaluate)
import           Test.Hspec            hiding (it)
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck
import           Lambda.TestUtils      -- I'm redefining `it` to use 1000 examples
import           Data.List (dropWhileEnd )
import           Data.Char (isSpace)

import LambdaTerm
import LambdaCompiler
import LambdaInterpreter

-- `main` is here so that this module can be run from GHCi on its own.
-- It is not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

readEval :: String -> [(String, LTerm)] -> LTerm
readEval input env =
  case compileToLambda input of
    Right term -> case eval term env of
      Right result -> result
      Left _       -> error "don't care"
    Left  err      -> error "don't care"

trim = dropWhileEnd isSpace . dropWhile isSpace

spec :: Spec
spec =
  describe "eval" $ do
    it "evaluates booleans to themselves" $ property $ \bool -> readEval (show bool) [] === LBool bool
    it "evaluates ints to themselves" $ property $ \int -> readEval (show int) [] === LInt int
    it "evaluates atoms to their env values (LInt)" $
      property $ \int -> readEval "test" [("test", LInt int)] === LInt int
    it "evaluates atoms to their env values (LVar)" $
      property $ \string -> readEval "test" [("test", LVar string)] === LVar string
    it "evaluates atoms to their env values (LBool)" $
      property $ \bool -> readEval "test" [("test", LBool bool)] === LBool bool
    it "does not evaluate quoted values (ints)" $ property $ \int -> readEval ("'" ++ show int) [] === LInt int
    it "does not evaluate quoted values (atoms)" $ readEval "'test" [] === LVar "test"
    it "does not evaluate quoted values (lists)" $
      readEval "'(test 1 2 3)" [] === LList [LVar "test", LInt 1, LInt 2, LInt 3]
    it "evaluates lambda expression to themselves" $
      readEval "(lambda (n m) (+ n m))" [] === LAbs "n" (LAbs "m" (LBinPrimOp "+" (LVar "n") (LVar "m")))
    it "can add" $ readEval "(+ 2 3)" [] === LInt 5
    it "can substract" $ readEval "(- 5 3)" [] === LInt 2
    it "can multiply" $ readEval "(* 5 3)" [] === LInt 15
    it "can divide" $ readEval "(/ 100 5)" [] === LInt 20
    it "can compute remainders" $ readEval "(% 22 7)" [] === LInt 1 .&&. readEval "(% 21 7)" [] === LInt 0
    it "can test equality" $ readEval "(eq 100 5)" [] === LBool False .&&. readEval "(eq 79 79)" [] === LBool True
    it "can compare ints" $
      readEval "(leq 99 10)" [] === LBool False .&&. readEval "(leq 79 79)" [] === LBool True .&&.
      readEval "(leq 19 79)" [] ===
      LBool True
    it "can conjoin booleans" $
      property $ \b1 b2 -> readEval ("(and " ++ show b1 ++ " " ++ show b2 ++ ")") [] === LBool (b1 && b2)
    it "can disjoin booleans" $
      property $ \b1 b2 -> readEval ("(or " ++ show b1 ++ " " ++ show b2 ++ ")") [] === LBool (b1 || b2)
    it "can cons lists" $
      property $ \l1 l2 -> readEval ("(cons " ++ show l1 ++ " " ++ show l2 ++ ")") [] === LList [LInt l1, LInt l2]
    it "can take the head of a list" $
      readEval "(car '(1 2 3))" [] === LInt 1
    it "can take the tail of a list" $
      readEval "(cdr '(1 2 3))" [] === readEval "'(2 3)" []
    it "can apply anonymous functions to arguments" $
      readEval "((lambda (n m) (+ n m)) 7 8)" [] === LInt 15
    it "can apply primitive operations to arguments" $
      readEval "((car '(+ -)) 7 8)" [] === LInt 15