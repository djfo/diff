module Main where

import System.Environment

data Sym a = Sym a | Gap deriving (Eq, Show)

data Op = Replace | Delete | Insert | Match deriving (Eq, Show)

editTranscript :: Eq a => [Sym a] -> [Sym a] -> [Op]
editTranscript [] [] = []
editTranscript (x:xs) (y:ys) = op : editTranscript xs ys
  where
    op | x /= Gap && y == Gap = Delete
       | x == Gap && y /= Gap = Insert
       | x == y               = Match
       | x /= y               = Replace

showS :: [Sym Char] -> String
showS []     = []
showS (x:xs) = f x : showS xs
  where
    f (Sym y) = y
    f Gap     = '_'

showET :: [Op] -> String
showET = map f
  where
    f Replace = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'

toSym :: Char -> Sym Char
toSym x | x == '_'  = Gap
        | otherwise = Sym x

main :: IO ()
main = do
  [s, t] <- map (map toSym) <$> getArgs
  putStrLn (showS s)
  putStrLn (showS t)
  putStrLn (showET (editTranscript s t))
