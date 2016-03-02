module Aligned where

import Edit

data Sym a = Sym a | Gap deriving (Eq, Show)

type AlignedString a = [Sym a]

editTranscript :: Eq a => AlignedString a -> AlignedString a -> EditTranscript a
editTranscript [] [] = []
editTranscript (x:xs) (y:ys) = op : editTranscript xs ys
  where
    op | x /= Gap && y == Gap = Delete
       | x == Gap && y /= Gap = Insert
       | x == y               = Match
       | Sym x' <- x, Sym y' <- y, x' /= y' = Replace x' y'

showS :: [Sym Char] -> String
showS []     = []
showS (x:xs) = f x : showS xs
  where
    f (Sym y) = y
    f Gap     = '_'

toSym :: Char -> Sym Char
toSym x | x == '_'  = Gap
        | otherwise = Sym x
