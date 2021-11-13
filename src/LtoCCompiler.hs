module LtoCCompiler where

import Prelude hiding (abs, EQ)

data LTerm = Free  String
  | Bound String
  | LInt Int
  | Op String
  | Abs   (String, LTerm)
  | Apply (LTerm, LTerm) deriving (Eq, Show)

data Comb = I|K|S|B|C|Y|
              CONS|HD|TL|
              PLUS|MINUS|TIMES|DIV|
              IF|EQ|AND|OR|NOT|
              WAIT|WAIT1|COPY|
              PR|PAR|SET|
              DEF String deriving (Eq, Show)

data Value = Vint Int
           | Vreal Double
           | Vbool Bool deriving (Eq, Show)

data Snode = Satom Value
           | Scomb Comb
           | Sapp (Snode,Snode) deriving (Eq, Show)

abs :: String -> LTerm -> LTerm
abs x term@(Free y) = Apply(Op "K",term)
abs x term@(Bound y)= if x==y then Op"I" else Apply(Op "K",term)
abs x term@(LInt y) = Apply(Op "K",term)
abs x term@(Op y)   = Apply(Op "K",term)
abs x (Abs(y,body)) = abs x (abs y body)
abs x (Apply(a,b))  = Apply(Apply(Op "S",abs x a), abs x b)


mkComb :: String -> Comb
mkComb "K" = K
mkComb "I" = I
mkComb "S" = S
mkComb "B" = B
mkComb "C" = C
mkComb "Y" = Y
mkComb "CONS" = CONS
mkComb "HD" = HD
mkComb "TL" = TL
mkComb "ADD" = PLUS
mkComb "SUB" = MINUS
mkComb "MUL" = TIMES
mkComb "DIV" = DIV
mkComb "IF" = IF
mkComb "EQ" = EQ
mkComb "AND" = AND
mkComb "OR" = OR
mkComb "NOT" = NOT
mkComb "PR" = PR
mkComb str = DEF str


c :: LTerm -> Snode
c (Free a) = Sapp(Scomb(DEF a),Satom(Vint 0))
c (Bound a) = error $ "can compile (Bound " ++ show a ++ ")"
c (LInt a) = Satom(Vint a)
c (Op k) = Scomb(mkComb k)
c (Apply(a,b)) = Sapp(c a,c b)
c (Abs(x,body)) = c (abs x body);


opt :: Snode -> Snode
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e)),Scomb I)) = e
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e1)),Sapp(Scomb K,e2))) =
   Sapp(Scomb K,Sapp(e1,e2))
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e1)),e2)) =
   Sapp(Sapp(Scomb B,e1),e2)
opt (Sapp(Sapp(Scomb S,e1),Sapp(Scomb K,e2))) =
   Sapp(Sapp(Scomb C,e1),e2)
opt (Sapp(e1,e2)) = Sapp(opt e1,opt e2)
opt x = x

ropt :: Snode -> Snode
ropt x =
 let
  y = opt x
 in
  if y==x then x
  else ropt y

isapp :: Snode -> Bool
isapp (Sapp _) = True
isapp _        = False


toString :: Snode -> String
toString (Satom (Vint n))    = show n
toString (Satom (Vreal x))   = show x
toString (Satom (Vbool w))   = show w
toString (Scomb k)           = show k
toString (Sapp (rator,rand)) = toString rator ++ " " ++
  (if isapp rand
    then "(" ++ toString rand ++ ")"
    else toString rand)
