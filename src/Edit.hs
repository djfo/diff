module Edit (
    Op(..)
  , EditTranscript
  , showET
  , CostFunction
  , editDistance
  ) where

import Control.Arrow (second)
import Data.Array ((!))
import qualified Data.Array as A
import Data.List (minimumBy)
import Data.Function (on)

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
      | a ! (i - 1) == b ! (j - 1) = go (i - 1) (j - 1) Match
      | otherwise = minimumBy (compare `on` fst)
                      [ go (i - 1) j       Delete
                      , go i       (j - 1) Insert
                      , go (i - 1) (j - 1) (Replace (a ! (i - 1)) (b ! (j - 1)))
                      ]

    go i j op = let (score, script) = ds ! (i, j) in (score + cost op, op : script)

    a = A.listArray (0, m) s
    b = A.listArray (0, n) t

    ds = A.listArray bounds [d i j | (i, j) <- A.range bounds]
    bounds = ((0, 0), (m, n))
    (m, n) = (length s, length t)

showET :: [Op a] -> String
showET = map f
  where
    f (Replace _ _) = 'R'
    f Delete  = 'D'
    f Insert  = 'I'
    f Match   = 'M'
