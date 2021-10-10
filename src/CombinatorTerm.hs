{-# OPTIONS_GHC -fwarn-incomplete-patterns #-} -- ensure that all possible LTerms are covered in pattern matching
module CombinatorTerm where

data CombinatorTerm = 
    I CombinatorTerm                                                -- ^ I x = x
  | K CombinatorTerm CombinatorTerm                                 -- ^ K x y = x
  | S CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ S x y z = x z (y z)
  | B CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ B x y z = x (y z)
  | C CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ C x y z = x z y
  | W CombinatorTerm CombinatorTerm                                 -- ^ W x y = x y y
  | P CombinatorTerm CombinatorTerm CombinatorTerm CombinatorTerm   -- ^ P w x y z = (w x, y z)
  | CBinOp String BinOp CombinatorTerm CombinatorTerm
  | CUnyOp String UnyOp CombinatorTerm
  | CInt Integer
  | CBool Bool
  | CList [CombinatorTerm]
  | CFree String

-- | As the LTerm data type contains function elements (like LBinPrimOp) we can't use deriving (Show) but have to implement it manually      
instance Show CombinatorTerm where
  show (I x)                   = "I " ++ show x
  show (K x y)                 = "K " ++ show x ++ " " ++ show y
  show (S x y z)               = "S" ++ show x ++ " " ++ show y ++ " " ++ show z
  show (B x y z)               = "B" ++ show x ++ " " ++ show y ++ " " ++ show z
  show (C x y z)               = "C" ++ show x ++ " " ++ show y ++ " " ++ show z
  show (W x y)                 = "W" ++ show x ++ " " ++ show y
  show (P x1 y1 x2 y2)         = "P" ++ show x1 ++ " " ++ show y1 ++ " " ++ show x2 ++ " " ++ show y2
  show (CBinOp name _op t1 t2) = "CBinOp \"" ++ name ++ "\" (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (CUnyOp name _op t1)    = "CBinOp \"" ++ name ++ "\" (" ++ show t1 ++ ")"
  show (CInt i)                = show i
  show (CBool b)               = show b
  show (CList l)               = show l
  show (CFree x)               = x
  
type BinOp = CombinatorTerm -> CombinatorTerm -> CombinatorTerm
type UnyOp = CombinatorTerm -> CombinatorTerm

k :: BinOp
k x _y = x
  

