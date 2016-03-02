module Main where

import System.Environment

import Edit
import Print

data Sym a = Sym a | Gap deriving (Eq, Show)

editTranscript :: Eq a => [Sym a] -> [Sym a] -> EditTranscript a
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

diff :: FilePath -> FilePath -> IO ()
diff path1 path2 =
  do
    lines1 <- lines <$> readFile path1
    lines2 <- lines <$> readFile path2
    let t = editDistance cost lines1 lines2
    printEdit t lines1 lines2
  where
    cost :: Op a -> Int
    cost op =
      case op of
        Replace _ _ -> 2
        Match -> 0
        _ -> 1

strDiff :: IO ()
strDiff =
  do
    [s, t] <- getArgs
    let et = editDistance cost s t
    printEdit et s t
  where
    cost :: Op a -> Int
    cost op =
      case op of
        Replace _ _ -> 0
        Match -> 0
        _ -> 1

main :: IO ()
main = strDiff
