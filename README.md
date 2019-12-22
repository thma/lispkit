[![Actions Status](https://github.com/thma/lispkit/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/lispkit/actions)

# lispkit
Implementing a small functional language with a combinator graph reduction engine

## already implemented
- implement if based on bools 
- implement all lispkit primops
- have 'it' in the repl
- provide :l, :r and :q in repl
- add define to repl
- compile SEpxr to lambda terms
- build eval based on lambda terms
- eval can throw exceptions
- support closures with lexical / static scope
- write regression test suite

## todo list
- implement letrec in lambda term evaluator 
- compile lambda to SKI combinator
- write graph reduction for combinator graphs
- reimplement P combinator from my ancient webLisp
