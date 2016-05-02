{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Print where

import           Edit

import           Data.Text (Text)
import qualified Data.Text as T

class ToString a where
  toS :: a -> String

instance ToString Char where
  toS = (: [])

instance ToString String where
  toS = id

instance ToString Text where
  toS = T.unpack

red :: String -> String
red s = "\ESC[1;31m\ESC[1;4m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[1;32m\ESC[1;4m" ++ s ++ "\ESC[0m"

printEdit :: ToString a => EditTranscript a -> [a] -> [a] -> IO ()
printEdit (op:ops) xs ys =
  case op of
    Insert -> do
      putStrLn $ green (toS (head ys))
      printEdit ops xs (tail ys)
    Delete -> do
      putStrLn $ red (toS (head xs))
      printEdit ops (tail xs) ys
    Match -> do
      putStrLn $ toS (head xs)
      printEdit ops (tail xs) (tail ys)
    Replace x y -> do
      putStr $ red (toS x)
      putStr $ green (toS y)
      putStrLn []
      printEdit ops (tail xs) (tail ys)
printEdit _ [] [] = return ()
printEdit _ _ _ = error "invalid arguments"
