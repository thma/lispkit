module OldCombinatorCompiler where
  
import Prelude hiding (abs)  
  
{--
 Compiler  Lambda -> Kombinatorlogik
--} 

data Comb = I|K|S|B|C|Y|
            CONS|HD|TL|
            PLUS|MINUS|TIMES|DIV|
            IF|EQ|AND|OR|NOT|
            WAIT|WAIT1|COPY|
            PR|PAR|SET|
            DEF String

data Value = In Integer|
             Re Double |
             Bool Bool

data Snode = Satom Value
            |Scomb Comb
            |Sapp (Snode,Snode)


data Term = Free String
          | Bound String
          | Int Integer
          | Op String
          | Abs (String, Term)
          | Apply (Term,Term)



--exception Doublebind
abs :: String -> Term -> Term
abs _ (Free y)      = Apply(Op "K",Free y)
abs x (Bound y)     = if x==y then Op "I" else Apply(Op "K", Bound y)
abs _ (Int y)       = Apply(Op "K",Int y)
abs _ (Op y)        = Apply(Op "K",Op y)
abs x (Abs(y,body)) = abs x (abs y body)
abs x (Apply(a,b))  =
         Apply(Apply(Op "S", abs x a), abs x b)


{--

fun mkComb "I" = I
   |mkComb "K" = K
   |mkComb "S" = S
   |mkComb "B" = B
   |mkComb "C" = C
   |mkComb "Y" = Y
   |mkComb "CONS" = CONS
   |mkComb "HD" = HD
   |mkComb "TL" = TL
   |mkComb "ADD" = PLUS
   |mkComb "SUB" = MINUS
   |mkComb "MUL" = TIMES
   |mkComb "DIV" = DIV
   |mkComb "IF" = IF
   |mkComb "EQ" = EQ
   |mkComb "AND" = AND
   |mkComb "OR" = OR
   |mkComb "NOT" = NOT
   |mkComb "PR" = PR
   |mkComb str = DEF str;
 



  
exception Compile;
fun c (Free a) = sapp(scomb(DEF a),satom(int 0))
   |c (Bound a) = raise Compile 
   |c (Int a) = satom(int a)
   |c (Op k) = scomb(mkComb k)
   |c (Apply(a,b)) = sapp(c a,c b)
   |c (Abs(x,body)) = c (abs x body);



fun opt (sapp(sapp(scomb S,sapp(scomb K,e)),scomb I)) = (e : snode)
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),sapp(scomb K,e2))) =
       sapp(scomb K,sapp(e1,e2))
   |opt (sapp(sapp(scomb S,sapp(scomb K,e1)),e2)) =
       sapp(sapp(scomb B,e1),e2)
   |opt (sapp(sapp(scomb S,e1),sapp(scomb K,e2))) =
       sapp(sapp(scomb C,e1),e2)
   |opt (sapp(e1,e2)) = sapp(opt e1,opt e2)
   |opt x = x;

fun ropt x =
 let
  val y = opt x;
 in 
  if y=x then x
  else ropt y
 end;


 fun k2string I = "I"
   |k2string K = "K"
   |k2string S = "S"
   |k2string B = "B"
   |k2string C = "C"
   |k2string Y = "Y"
   |k2string CONS = "CONS"
   |k2string HD = "HD"
   |k2string TL = "TL"
   |k2string PLUS = "PLUS"
   |k2string MINUS = "MINUS"
   |k2string TIMES = "TIMES"
   |k2string DIV = "DIV"
   |k2string IF = "IF"
   |k2string EQ = "EQ"
   |k2string AND = "AND"
   |k2string OR = "OR"
   |k2string NOT = "NOT"
   |k2string WAIT = "WAIT"
   |k2string WAIT1 = "WAIT1"
   |k2string COPY = "COPY"
   |k2string PR = "PR"
   |k2string PAR = "SET"
   |k2string (DEF(name)) = name;
   
fun isapp (sapp _) = true
   |isapp _ = false;

fun show (satom (int n)) = makestring n
   |show (satom (re x)) =makestring x
   |show (satom (bool w)) = makestring w
   |show (scomb k) = k2string k
   |show (sapp (rator,rand)) = (show rator)^" "^(if (isapp rand) 
				then "("^(show rand)^")"
				else (show rand));



--}

