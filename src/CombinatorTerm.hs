module CombinatorTerm where

data CombinatorTerm = 
    I CombinatorTerm                                                -- ^ I x = x
  | K CombinatorTerm CombinatorTerm                                 -- ^ K x y = x
  | S CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ S x y z = x z (y z)
  | B CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ B x y z = x (y z)
  | C CombinatorTerm CombinatorTerm CombinatorTerm                  -- ^ C x y z = x z y
  | W CombinatorTerm CombinatorTerm                                 -- ^ W x y = x y y
  | P CombinatorTerm CombinatorTerm CombinatorTerm CombinatorTerm   -- ^ P w x y z = (w x, y z)
  | CInt Integer
  | CBool Bool
  | CList [CombinatorTerm]
  | CFree String
  

