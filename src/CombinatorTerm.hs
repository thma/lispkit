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
  
  
type BinOp = CombinatorTerm -> CombinatorTerm -> CombinatorTerm
type UnyOp = CombinatorTerm -> CombinatorTerm

k :: BinOp
k x _y = x
  

