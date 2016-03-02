module Edit where

import Data.List (sortOn)

data Op a = Replace a a | Delete | Insert | Match deriving (Eq, Show)

type EditTranscript a = [Op a]

edits :: Eq a => [a] -> [a] -> [(Op a, ([a], [a]))]
edits [] []     = []
edits [] (_:ys) = [(Insert, ([], ys))]
edits (_:xs) [] = [(Delete, (xs, []))]
edits (x:xs) (y:ys) = [(Insert, (x:xs, ys)), (Delete, (xs, y:ys)), (subst, (xs, ys))]
  where
    subst | x == y    = Match
          | otherwise = Replace x y

type CostFunction a = Op a -> Int

editDistance :: Eq a => CostFunction a -> [a] -> [a] -> EditTranscript a
editDistance _ [] [] = []
editDistance cost xs ys = edit : uncurry (editDistance cost) args
  where
    (edit, args) : _ = sortOn (cost . fst) (edits xs ys)

showET :: [Op a] -> String
showET = map f
  where
    f (Replace _ _) = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'
