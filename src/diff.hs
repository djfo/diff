module Main where

import Data.List (sortOn)
import System.Environment

data Sym a = Sym a | Gap deriving (Eq, Show)

data Op a = Replace a a | Delete | Insert | Match deriving (Eq, Show)

type EditTranscript a = [Op a]

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

showET :: [Op a] -> String
showET = map f
  where
    f (Replace _ _) = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'

toSym :: Char -> Sym Char
toSym x | x == '_'  = Gap
        | otherwise = Sym x

cost :: Op a -> Int
cost op =
  case op of
    Replace _ _ -> 0
    Match -> 0
    _ -> 1

edits :: Eq a => [a] -> [a] -> [(Op a, ([a], [a]))]
edits [] (y:ys) = [(Insert, ([], ys))]
edits (x:xs) [] = [(Delete, (xs, []))]
edits (x:xs) (y:ys) = [(Insert, (x:xs, ys)), (Delete, (xs, y:ys)), (subst, (xs, ys))]
  where
    subst | x == y = Match
          | x /= y = Replace x y

editDistance :: Eq a => [a] -> [a] -> EditTranscript a
editDistance [] [] = []
editDistance xs ys = e : uncurry editDistance rec
  where
    (e, rec) : _ = sortOn (cost . fst) (edits xs ys)

diff :: FilePath -> FilePath -> IO ()
diff path1 path2 = do
  lines1 <- lines <$> readFile path1
  lines2 <- lines <$> readFile path2
  putStrLn $ showET (editDistance lines1 lines2)

main :: IO ()
main = do
  [s, t] <- getArgs
  putStrLn $ showET (editDistance s t)
