module Main where

import LambdaCompiler
import LispkitParser
import LispkitInterpreter

main :: IO ()
main = do
  let input = "(cadr (cons 78 (- n 89)))"
  let output = readSExpr input
  print output
  case output of
    Right x  -> print $ eval x [("n", SInt 100)]
    Left err -> print err
