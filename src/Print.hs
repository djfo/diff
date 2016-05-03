{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Print where

import           Edit
import           Tok.C

import           Control.Applicative ((<*))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Text.Parsec         as P

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

printEdit' :: Bool -> EditTranscript Text -> [Text] -> [Text] -> IO ()
printEdit' isLine (op:ops) xs ys =
  case op of
    Insert -> do
      p $ green (toS (head ys))
      go ops xs (tail ys)
    Delete -> do
      p $ red (toS (head xs))
      go ops (tail xs) ys
    Match -> do
      p $ toS (head xs)
      go ops (tail xs) (tail ys)
    Replace x y -> do
      case (P.parse (cProg <* P.eof) "" x, P.parse (cProg <* P.eof) "" y) of
        (Right x', Right y') -> do
          let (_, t) = editDistance stdCost x' y'
          printEdit' True t x' y'
          putStrLn []
        _ -> do
          p $ red (toS x)
          p $ green (toS y)
      printEdit ops (tail xs) (tail ys)
  where
    p = if isLine then putStr else putStrLn
    go = printEdit' isLine

printEdit' _ _ [] [] = return ()
printEdit' _ _ _ _ = error "invalid arguments"

printEdit :: EditTranscript Text -> [Text] -> [Text] -> IO ()
printEdit = printEdit' False
