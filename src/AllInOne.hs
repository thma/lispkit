{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module AllInOne where

import Data.List
import Data.Maybe
import Text.Parsec
import Data.Functor.Identity (Identity)
import           System.IO            (hSetEncoding, stdin, stdout, utf8)

type Parser = Parsec String ()

infixl 5 :@
data Expr =
    Expr :@ Expr
  | Var String
  | Int Integer
  | Lam String Expr
  deriving (Eq, Show)

type Environment = [(String, Expr)]

num :: Parser Expr
num = do
  sign   <- many (oneOf "-")
  digits <- many1 digit <* ws
  case length sign of
    0 -> return $ Int (read digits)
    _ -> return $ Int (read $ "-" ++ digits)

source :: Parser Environment
source = catMaybes <$> many maybeLet where
  maybeLet :: ParsecT String () Identity (Maybe (String, Expr))
  maybeLet = between ws newline $ optionMaybe $ (,) <$> var <*> (str "=" >> term)
  term :: ParsecT String () Identity Expr
  term = try num
     <|> lam
     <|> app
  lam :: ParsecT String () Identity Expr
  lam = flip (foldr Lam) <$> between lam0 lam1 (many1 var) <*> term where
    lam0 = str "\\" <|> str "\0955"
    lam1 = str "->" <|> str "."
  app :: ParsecT String () Identity Expr
  app = foldl1' (:@) <$> many1
    (
     try num
     <|> Var <$> var
     <|> between (str "(") (str ")") term )

  var :: ParsecT String u Identity String
  var   = (mathOp <|> many1 alphaNum) <* ws

  mathOp :: ParsecT String u Identity String
  mathOp = string "+" <|> string "/" <|> string "*" -- <|> string "-"

  str = (>> ws) . string

ws :: ParsecT String u Identity ()
ws = many (oneOf " \t") >> optional (try $ string "--" >> many (noneOf "\n"))


fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f
fv vs _                     = vs

babs :: Environment -> Expr -> Expr
babs env (Lam x e)
  | Var "i" :@ x <- t                            = t
  | Var "s" :@ Var"k" :@ _ <- t                  = Var "s" :@ Var "k"
  | x `notElem` fv [] t                          = Var "k" :@ t
  | Var y <- t, x == y                           = Var "i" -- Var "s" :@  Var "k" :@ Var "k"
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y :@ m :@ Var z <- t, x == y, x == z     = babs env $ Lam x $ Var "s" :@ Var "s" :@ Var "k" :@ Var x :@ m
  | m :@ (n :@ l) <- t, isComb m, isComb n       = babs env $ Lam x $ Var "s" :@ Lam x m :@ n :@ l
  | (m :@ n) :@ l <- t, isComb m, isComb l       = babs env $ Lam x $ Var "s" :@ m :@ Lam x l :@ n
  | (m :@ l) :@ (n :@ l') <- t,
     l `noLamEq` l', isComb m, isComb n          = babs env $ Lam x $ Var "s" :@ m :@ n :@ l
  | m :@ n <- t                                  = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  where t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s
babs env (m :@ n)          = babs env m :@ babs env n
babs env x                 = x

isComb :: Expr -> Bool
isComb e = null $ fv [] e \\ ["s", "k"]

noLamEq :: Expr -> Expr -> Bool
noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False


opt :: Expr -> Expr
--opt (Var "i" :@ n@(Int _n)) = n
--opt ((Var "s" :@ e1) :@ (Var "k" :@ e2)) = (Var "c" :@ e1) :@ e2

--opt (x :@ y) = opt x :@ opt y
opt x = x

ropt :: Expr -> Expr
ropt expr =
  let expr' = opt expr
  in  if expr' == expr
        then expr
        else ropt expr'

{-
fun opt (sapp(sapp(scomb S,sapp(scomb K,e)),scomb I)) = (e : snode)
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),sapp(scomb K,e2))) =
       sapp(scomb K,sapp(e1,e2))
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),e2)) =
       sapp(sapp(scomb B,e1),e2)
   |opt (sapp(sapp(scomb S,e1),sapp(scomb K,e2))) =
       sapp(sapp(scomb C,e1),e2)
   |opt (sapp(e1,e2)) = sapp(opt e1,opt e2)
   |opt x = x;

-}

-- graph allocation
type Pointer = Int

data Graph =
    Node Pointer Pointer
  | Comb String
  | Num  Integer
  deriving (Eq, Show)

type AllocatedGraph = [(Pointer, Graph)]

collectAllReachables :: Pointer -> AllocatedGraph -> AllocatedGraph -> AllocatedGraph
collectAllReachables rootP aGraph result =
  let rootNode = peek rootP aGraph
  in case rootNode of
    Node l r -> (l, peek l aGraph) : (r, peek r aGraph) : collectAllReachables l aGraph result ++ collectAllReachables r aGraph result ++ result
    Comb s -> result
    Num n -> result


compactify :: Pointer -> AllocatedGraph -> AllocatedGraph
compactify rootP aGraph = (rootP, peek rootP aGraph) : collectAllReachables rootP aGraph []


allocate :: Expr -> AllocatedGraph
allocate expr =
  alloc expr 1 []
  where
    maxPointer :: AllocatedGraph -> Pointer
    maxPointer x = maximum $ map fst x

    alloc :: Expr -> Int -> [(Int, Graph)] -> [(Int, Graph)]
    alloc (Var name) pointer memMap = (pointer, Comb name) : memMap
    alloc (Int val)  pointer memMap = (pointer, Num val) : memMap
    alloc (l :@ r)   pointer memMap =
      let pointerL = pointer+1
          allocL   = alloc l pointerL []
          maxL     = maxPointer allocL
          pointerR = maxL + 1
          allocR   = alloc r pointerR []
          maxR     = maxPointer allocR
      in
      (pointer, Node pointerL pointerR) : (allocL ++ allocR ++ memMap)
    alloc (Lam _ _)  pointer memMap = error "lambdas should already be abstracted"


spine :: Pointer -> AllocatedGraph -> [(Pointer, Graph)] -> (Graph, [(Pointer, Graph)])
spine rootP graph stack =
  case peek rootP graph of
    c@(Comb _)   -> (c, stack)
    n@(Num _)    -> (n, stack)
    g@(Node l r) -> spine l graph ((rootP,g):stack)


run :: String -> IO Graph
run source = do
  let sk = getSK . toSK $ source
      g  = allocate sk
  print sk
  print g
  print (spine 1 g [])
  return $ snd(head(loop 1 g))

loop :: Pointer -> AllocatedGraph -> AllocatedGraph
loop rootP aGraph =
  let aGraph' = compactify rootP (step rootP aGraph)
  in  if aGraph == aGraph' 
        then aGraph
        else loop rootP aGraph'

step :: Pointer -> AllocatedGraph -> AllocatedGraph
step rootP graph =
  let root = peek rootP
      (g,stack)   = spine rootP graph []
  in  case g of
        (Comb k) -> apply k stack graph rootP
        _        -> graph

apply :: String -> [(Pointer, Graph)] -> AllocatedGraph-> Pointer -> AllocatedGraph
apply "i" ((p,Node _ xPointer):_) aGraph rootP =
  let xVal = peek xPointer aGraph
  in  poke p xVal aGraph
apply "k" ((_p, Node _ xPointer):(p, Node _ _):_) aGraph rootP =
  poke p (peek xPointer aGraph) aGraph

apply k _ _ _ = error $ "undefined combinator " ++ k 

  --  |apply (I,(node as ref(app((_,ref x),_,ref q)))::_) =
  --       (node := x; set_q node q)
  --  |apply (K,ref(app((_,ref x),_,ref q))::(node as ref(app(_,_,_)))::_) =
  --       (node := x; set_q node q)
  --  |apply (S,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  --       node := app((ref(app((x,z),ref Eval,q)),
  --                    ref(app((y,z),ref Eval,q))),
  --                   ref Eval,q)
  --  |apply (B,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  --       node := app((x,ref (app((y,z),ref Eval,q))),ref Eval,q)
  --  |apply (C,(ref(app((_,x),_,_)))::(ref(app((_,y),_,_)))
  --             ::(node as (ref(app((_,z),m,q))))::_) =
  -- node := app((ref(app((x,z),ref Eval,q)),y),ref Eval,q)

  --  |apply (Y,(node as ref(app((_,f),m,q)))::_) =
  --       node := app((f,node),ref Eval,q)
  --  |apply (DEF(name),(node as ref(app((_,_),_,_)))::_) =
  --       node := !(copy(lookup name))
  --  |apply (PLUS,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --               ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x+y),ref Ready,q)
  --  |apply (PLUS,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (MINUS,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --                ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x-y),ref Ready,q)
  --  |apply (MINUS,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (TIMES,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --                ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x*y),ref Ready,q)
  --  |apply (TIMES,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (DIV,ref(app((_,ref(atom(int x,_,_))),_,_))::(node as 
  --              ref(app((_,ref(atom(int y,_,_))),_,q)))::_) =
  --       node := atom(int(x div y),ref Ready,q)
  --  |apply (DIV,(stack as ref(app((_,x),_,_))::
  --                         ref(app((_,y),_,_))::_)) =
  --       (subEval (last stack,x);
  --        subEval (last stack,y); ())
  --  |apply (EQ,(stack as ref(app((_,x),_,_))::(node as
  --                         ref(app((_,y),_,q)))::_)) =
  --       if (!(get_mark x)) = Ready andalso
  --          (!(get_mark y)) = Ready 
  --         then node := atom(bool(equal x y),ref Ready,q)
  --       else
  --         (subEval (last stack,x);
  --         subEval (last stack,y); ())
  --  |apply (IF,(ref(app((_,ref(atom(bool test,_,_))),_,_))):: 
  --             (ref(app((_,x),_,_)))::(node as (ref(app((_,y),_,_))))::_) =
  --       if test then node := !x
  --       else node := !y
  --  |apply (IF,(stack as (ref(app((_,test),_,_)):: 
  --             ref(app((_,x),_,_))::(node as ref(app((_,y),_,q)))::_))) =
  --       subEval (last stack,test)


peek :: Pointer -> AllocatedGraph -> Graph
peek pointer graph = fromMaybe (error "merde") (lookup pointer graph)

poke :: Pointer -> Graph -> AllocatedGraph -> AllocatedGraph
poke key value assoc = (key,value):filter ((key /=).fst) assoc


-- parse a lambda expression
toSK :: String -> Either ParseError Expr
toSK s = do
  env <- parse source "" (s ++ "\n")
  case lookup "main" env of
    Nothing -> Left $ error "missing main function!"
    Just t -> pure $ ropt $ babs env t

getSK :: Either ParseError Expr -> Expr
getSK (Right exp) = exp
getSK _           = Var "error"

red :: Expr -> Expr
red i@(Int _i) = i
red (Var "i" :@ x) = x
red (Var "i" :@ x :@ y) = x :@ y
red (Var "k" :@ x :@ _) = x
red (Var "k" :@ x :@ _ :@ z) = x :@ z
red (Var "s" :@ f :@ g :@ x) = f :@ x :@ (g :@ x)
red (Var "s" :@ f :@ g :@ x :@ z) = f :@ x :@ (g :@ x) :@ z
red (Var "c" :@ f :@ g :@ x) = f :@ x :@ g
red (Var "b" :@ f :@ g :@ x) = f :@  (g :@ x)
red (Var "is0" :@ x)    = if reduce x == Int 0 then Int 1 else Int 0
red (Var "sub1" :@ x)   = let (Int x') = reduce x in Int (x' - 1)
red (Var "+" :@ x :@ y) = mathReduce x y (+)
red (Var "-" :@ x :@ y) = mathReduce x y (-)
red (Var "*" :@ x :@ y) = mathReduce x y (*)
red (Var "/" :@ x :@ y) = mathReduce x y div
red (Var "if" :@ pred :@ thenPart :@ elsePart) =
  if reduce pred == Int 1
    then thenPart
    else elsePart
-- fallback:
red x = x

--(C f g x) = ((f x) g)
--(B f g x) = (f (g x))
--(S f g x) = (f x (g x))


mathReduce :: Expr -> Expr -> (Integer -> Integer -> Integer) -> Expr
mathReduce x y f =
  let (Int x') = reduce x
      (Int y') = reduce y
  in  Int (x' `f` y')

reduce :: Expr -> Expr
reduce expr =
  let expr' = red expr
  in  if expr' == expr
      then expr
      else reduce expr'

showSK :: Expr -> String
showSK (Var s)  = s ++ " "
showSK (x :@ y) = showSK x ++ showR y where
  showR (Var s) = s ++ " "
  showR _       = "(" ++ showSK y ++ ")"
showSK x        = show x ++ " "

main :: IO ()
main = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  --putStrLn testSource
  case toSK testSource of
    Left err -> print $ "error: " ++ show err
    Right sk -> do
      --putStrLn $ "compiled to SKI: " ++ showSK sk
      --putStrLn $ "as graph: " ++ show sk
      --putStrLn $ "reduce: "   ++ show (reduce sk)
      let sk = getSK (toSK "main = i 23")  --(toSK "main = c i 2 (+ 1)")
      print sk
      let g = allocate sk
      print g
      print $ spine 1 g []


      -- putStrLn $ "encoded: " ++ show (I.fromAscList $ zip [0..] $ encodeTree sk)
      -- putStrLn $ "run it: " ++ show (run (I.fromAscList $ zip [0..] $ encodeTree sk) [4])

--testSource =
--     "f = \\x y -> + x 3 \n"
--  ++ "g = λx. * x 7\n"
--  ++ "fact = λn. if (is0 n) 1 (* n (fact (sub1 n))) \n"
--  ++ "compose = λf g. f g \n"
--  ++ "main = fact 7"

-- "main = c i 2 (+ 1)"
-- "main = s k i 4 \n"

testSource =
   "Y = λf . (λx. x x)(λx . f(x x)) \n" ++
   "fact = Y(λf n. if (is0 n) 1 (* n (f (sub1 n)))) \n" ++
   "main = fact 4 \n"


--testSource = "id = \\x -> x \n" ++
--             "1 = \\f x -> f x \n" ++
--             "main = id 1"

-- testSource = "true = \\x y -> x \n" ++
--             "false = \\x y -> y \n" ++
--             "0 = \\f x -> x \n" ++
--             "1 = \\f x -> f x \n" ++
--             "succ = \\n f x -> f(n f x) \n" ++
--             "pred = \\n f x -> n(\\g h -> h (g f)) (\\u -> x) (\\u ->u) \n" ++
--             "mul = \\m n f -> m(n f) \n" ++
--             "is0 = \\n -> n (\\x -> false) true \n" ++
--             "Y = \\f -> (\\x -> x x)(\\x -> f(x x)) \n" ++
--             "fact = Y(\\f n -> (is0 n) 1 (mul n (f (pred n)))) \n" ++
--             "main = fact (succ (succ (succ 1))) \n"



