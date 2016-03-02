module List where

commonPrefixLen :: Eq a => [a] -> [a] -> Int
commonPrefixLen (x:xs) (y:ys)
  | x == y    = 1 + commonPrefixLen xs ys
  | otherwise = 0
commonPrefixLen _ _ = 0
