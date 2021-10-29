{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-} 
module AllInOne where

import System.Console.Haskeline
--import Data.Char
import qualified Data.IntMap as I
import Data.List
import Data.Maybe
import Text.Parsec

type Parser = Parsec String () 

infixl 5 :@
data Expr = 
    Expr :@ Expr 
  | Var String 
  | Lam String Expr

source :: Parser [(String, Expr)]
source = catMaybes <$> many maybeLet where
  maybeLet = between ws newline $ optionMaybe $ (,) <$> v <*> (str "=" >> term)
  term = lam <|> app
  lam = flip (foldr Lam) <$> between lam0 lam1 (many1 v) <*> term where
    lam0 = str "\\" <|> str "\0955"
    lam1 = str "->" <|> str "."
  app = foldl1' (:@) <$> many1
    ((Var <$> v) <|> between (str "(") (str ")") term)
  v   = many1 alphaNum <* ws
  str = (>> ws) . string
  ws = many (oneOf " \t") >> optional (try $ string "--" >> many (noneOf "\n"))  

fv :: [String] -> Expr -> [String]
fv vs (Var s) | s `elem` vs = []
              | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f

babs0 :: [(String, Expr)] -> Expr -> Expr
babs0 env (Lam x e)
  | Var y <- t, x == y  = Var "s" :@ Var "k" :@ Var "k"
  | x `notElem` fv [] t = Var "k" :@ t
  | m :@ n <- t         = Var "s" :@
    babs0 env (Lam x m) :@ babs0 env (Lam x n)
  where t = babs0 env e
babs0 env (Var s)
  | Just t <- lookup s env = babs0 env t
  | otherwise              = Var s
babs0 env (m :@ n) = babs0 env m :@ babs0 env n


babs :: [(String, Expr)] -> Expr -> Expr
babs env (Lam x e)
  | Var "s" :@ Var"k" :@ _ <- t = Var "s" :@ Var "k"
  | x `notElem` fv [] t = Var "k" :@ t
  | Var y <- t, x == y  = Var "s" :@  Var "k" :@ Var "k"
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y :@ m :@ Var z <- t, x == y, x == z =
    babs env $ Lam x $ Var "s" :@ Var "s" :@ Var "k" :@ Var x :@ m
  | m :@ (n :@ l) <- t, isComb m, isComb n =
    babs env $ Lam x $ Var "s" :@ Lam x m :@ n :@ l
  | (m :@ n) :@ l <- t, isComb m, isComb l =
    babs env $ Lam x $ Var "s" :@ m :@ Lam x l :@ n
  | (m :@ l) :@ (n :@ l') <- t, l `noLamEq` l', isComb m, isComb n
    = babs env $ Lam x $ Var "s" :@ m :@ n :@ l
  | m :@ n <- t        = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  where t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s
babs env (m :@ n) = babs env m :@ babs env n

isComb :: Expr -> Bool
isComb e = null $ fv [] e \\ ["s", "k"]

noLamEq :: Expr -> Expr -> Bool
noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False


toSK :: [Char] -> Either ParseError Expr
toSK s = do
  env <- parse source "" (s ++ "\n")
  case lookup "main" env of
    Nothing -> Left $ error "missing main"
    Just t -> pure $ babs env t :@ Var "u" :@ Var "z"
    
toArr :: Int -> Expr -> [Int]
toArr n (Var "z") = [0]
toArr n (Var "u") = [1]
toArr n (Var "k") = [2]
toArr n (Var "s") = [3]
toArr n (x@(Var _) :@ y@(Var _)) = toArr n x ++ toArr n y
toArr n (x@(Var _) :@ y)         = toArr n x ++ [n + 2] ++ toArr (n + 2) y
toArr n (x         :@ y@(Var _)) = n + 2 : toArr n y ++ toArr (n + 2) x
toArr n (x         :@ y)         = [n + 2, nl] ++ l ++ toArr nl y
  where l  = toArr (n + 2) x
        nl = n + 2 + length l

encodeTree :: Expr -> [Int]
encodeTree e = concatMap f $ 0 : toArr 4 e where
  f n | n < 4     = [n, 0, 0, 0]
      | otherwise = toU32 $ (n - 3) * 4

toU32 :: Int -> [Int]
toU32 = take 4 . byteMe

byteMe :: Integral t => t -> [t]
byteMe n | n < 256   = n : repeat 0
         | otherwise = n `mod` 256 : byteMe (n `div` 256)
         
         
run :: Num p => I.IntMap Int -> [Int] -> p
run m (p:sp) = case p of
  0 -> 0
  1 -> 1 + run m (arg 0 : sp)
  2 -> run m $ arg 0 : drop 2 sp
  3 -> run m' $ hp:drop 2 sp where
    m' = insList m $
      zip [hp..]    (concatMap toU32 [arg 0, arg 2, arg 1, arg 2]) ++
      zip [sp!!2..] (concatMap toU32 [hp, hp + 8])
    hp = I.size m
  _ -> run m $ get p:p:sp
  where
  arg k = get (sp!!k + 4)
  get n = sum $ zipWith (*) ((m I.!) <$> [n..n+3]) ((256^) <$> [0..3])
  insList = foldr (\(k, a) m -> I.insert k a m)
  
  
showSK :: Expr -> [Char]
showSK (Var s)  = s
showSK (x :@ y) = showSK x ++ showR y where
  showR (Var s) = s
  showR _       = "(" ++ showSK y ++ ")"

main :: IO ()
main = do
  input <- readFile "lambda.src"
  putStrLn $ case toSK input of
    Left err -> "error: " ++ show err
    Right sk -> unlines
      [ showSK sk
  --    , show $ compile $ encodeTree sk
      , show $ run (I.fromAscList $ zip [0..] $ encodeTree sk) [4]
      ]

expr :: Parser Expr
expr = foldl1 (:@) <$>
  many1 ((Var . pure <$> letter) <|> between (char '(') (char ')') expr)

skRepl :: InputT IO ()
skRepl = do
  ms <- getInputLine "> "
  case ms of
    Nothing -> outputStrLn ""
    Just s  -> do
      let Right e = parse expr "" s
      outputStrLn $ show $ encodeTree e
      --outputStrLn $ show $ compile $ encodeTree e
      outputStrLn $ show $ run (I.fromAscList $ zip [0..] $ encodeTree e) [4]
      skRepl       