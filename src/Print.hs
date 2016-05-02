{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Print where

import Edit

class ToString a where
  toS :: a -> String

instance ToString Char where
  toS = (: [])

instance ToString String where
  toS = id

printEdit :: ToString a => EditTranscript a -> [a] -> [a] -> IO ()
printEdit (op:ops) xs ys =
  case op of
    Insert -> do
      putStrLn $ "+" ++ toS (head ys)
      printEdit ops xs (tail ys)
    Delete -> do
      putStrLn $ "-" ++ toS (head xs)
      printEdit ops (tail xs) ys
    Match -> do
      putStrLn $ " " ++ toS (head xs)
      printEdit ops (tail xs) (tail ys)
    Replace x y -> do
      putStrLn $ "-" ++ toS x
      putStrLn $ "+" ++ toS y
      printEdit ops (tail xs) (tail ys)
printEdit _ [] [] = return ()
printEdit _ _ _ = error "invalid arguments"
