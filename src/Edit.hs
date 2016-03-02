module Edit where

import Data.List (sortOn)

data Op a = Replace a a | Delete | Insert | Match deriving (Eq, Show)

type EditTranscript a = [Op a]

edits :: Eq a => [a] -> [a] -> [(Op a, ([a], [a]))]
edits [] (y:ys) = [(Insert, ([], ys))]
edits (x:xs) [] = [(Delete, (xs, []))]
edits (x:xs) (y:ys) = [(Insert, (x:xs, ys)), (Delete, (xs, y:ys)), (subst, (xs, ys))]
  where
    subst | x == y = Match
          | x /= y = Replace x y

type CostFunction a = Op a -> Int

editDistance :: Eq a => CostFunction a -> [a] -> [a] -> EditTranscript a
editDistance cost [] [] = []
editDistance cost xs ys = e : uncurry (editDistance cost) rec
  where
    (e, rec) : _ = sortOn (cost . fst) (edits xs ys)

showET :: [Op a] -> String
showET = map f
  where
    f (Replace _ _) = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'
