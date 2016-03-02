{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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

class ToString a where
  toS :: a -> String

instance ToString Char where
  toS = (: [])

instance ToString String where
  toS = id

printEdit :: ToString a => EditTranscript a -> [a] -> [a] -> IO ()
printEdit _ [] [] = return ()
printEdit (op:ops) xs ys =
  case op of
    Insert -> do
      putStrLn $ "+ | " ++ toS (head ys)
      printEdit ops xs (tail ys)
    Delete -> do
      putStrLn $ "- | " ++ toS (head xs)
      printEdit ops (tail xs) ys
    Match -> do
      putStrLn $ "= | " ++ toS (head xs)
      printEdit ops (tail xs) (tail ys)
    Replace x y -> do
      putStrLn $ "R | " ++ toS x
      putStrLn $ "  | " ++ toS y
      printEdit ops (tail xs) (tail ys)

diff :: IO ()
diff = do
  [path1, path2] <- getArgs
  lines1 <- lines <$> readFile path1
  lines2 <- lines <$> readFile path2
  let t = editDistance lines1 lines2
  printEdit t lines1 lines2

strDiff :: IO ()
strDiff = do
  [s, t] <- getArgs
  let et = editDistance s t
  printEdit et s t

main :: IO ()
main = strDiff
