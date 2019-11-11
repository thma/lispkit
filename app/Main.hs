module Main where

import LambdaCompiler
import LispkitParser
import LispkitInterpreter

main :: IO ()
main = do
  let input = "(+ n 89)"
  let output = readSExpr input
  print output
  case output of
    Right x  -> print $ eval x [("n", SInt 23)]
    Left err -> print $ err
