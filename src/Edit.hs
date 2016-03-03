module Edit (
    Op(..)
  , EditTranscript
  , showET
  , CostFunction
  , editDistance
  , greedyEditDistance
  ) where

import Data.Array (listArray, (!), range)
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

greedyEditDistance :: Eq a => CostFunction a -> [a] -> [a] -> EditTranscript a
greedyEditDistance _ [] [] = []
greedyEditDistance cost xs ys = edit : uncurry (greedyEditDistance cost) args
  where
    (edit, args) : _ = sortOn (cost . fst) (edits xs ys)

-- TODO: compute bottom up, memoize
editDistance :: Eq a => CostFunction a -> [a] -> [a] -> (Int, EditTranscript a)
editDistance cost xs ys =
  case sortOn fst (map f (edits xs ys)) of
    []         -> (0, [])
    (c, t) : _ -> (c, t)
  where
    f (op, args) = let (d, t) = uncurry (editDistance cost) args in (cost op + d, op : t)

editDist :: Eq a => [a] -> [a] -> Int
editDist s t = d m n
  where
    d i 0 = i
    d 0 j = j
    d i j
      | s !! (i - 1) == t !! (j - 1) = ds ! (i - 1, j - 1) -- MATCH
      | otherwise = minimum [ ds ! (i - 1, j)     + 1 -- DELETE
                            , ds ! (i,     j - 1) + 1 -- INSERT
                            , ds ! (i - 1, j - 1) + 1 -- REPLACE
                            ]
    ds = listArray bounds [d i j | (i, j) <- range bounds]
    bounds = ((0, 0), (m, n))
    (m, n) = (length s, length t)

showET :: [Op a] -> String
showET = map f
  where
    f (Replace _ _) = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'
