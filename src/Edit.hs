module Edit (
    Op(..)
  , EditTranscript
  , CostFunction
  , editDistance
  , stdCost
  ) where

import           Control.Arrow (second)
import           Data.Array    ((!))
import qualified Data.Array    as A
import           Data.Function (on)
import           Data.List     (minimumBy)

data Op a = Replace a a | Delete | Insert | Match deriving (Eq, Show)

type EditTranscript a = [Op a]

type CostFunction a = Op a -> Int

editDistance :: Eq a => CostFunction a -> [a] -> [a] -> (Int, EditTranscript a)
editDistance cost s t = second reverse (d m n)
  where
    d 0 0 = (0, [])
    d i 0 = go (i - 1) 0 Delete
    d 0 j = go 0 (j - 1) Insert
    d i j
      | a ! i == b ! j = go (i - 1) (j - 1) Match
      | otherwise = minimumBy (compare `on` fst)
                      [ go (i - 1) j       Delete
                      , go i       (j - 1) Insert
                      , go (i - 1) (j - 1) (Replace (a ! i) (b ! j))
                      ]

    go i j op = let (score, script) = ds ! (i, j) in (score + cost op, op : script)

    a = A.listArray (1, m) s
    b = A.listArray (1, n) t

    ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
    bounds = ((0, 0), (m, n))
    (m, n) = (length s, length t)

stdCost :: Op a -> Int
stdCost op =
  case op of
    Match       -> 0
    Replace _ _ -> 2
    Insert      -> 1
    Delete      -> 1
