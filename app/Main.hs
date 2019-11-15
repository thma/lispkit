module Main where

import LambdaCompiler
import LispkitParser
import LispkitInterpreter

main :: IO ()
main = do
  let input = "(let (fac 10) (fac (lambda (n) (if (eq n 0) 1 (* n (fac (- n 1))))) ))"
  --"((lambda (f n) (if (eq n 0) 1 (* n (f f (- n 1))))) (lambda (f n) (if (eq n 0) 1 (* n (f f (- n 1))))) 10)"  --"(cadr (cons 78 (- n 89)))"
  let output = readSExpr input
  print output
  case output of
    Right x  -> print $ eval x [("n", SInt 100)]
    Left err -> print err
