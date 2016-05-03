{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Print (printEdit, zipET) where

import qualified Edit                as E
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

data DiffOp a
  = Id a
  | Insert a
  | Delete a
  | Replace a a
  deriving (Eq, Ord, Show)

zipET :: E.EditTranscript a -> [a] -> [a] -> [DiffOp a]
zipET (E.Insert : xs)      ys     (z:zs) = Insert z    : zipET xs ys zs
zipET (E.Delete : xs)      (y:ys) zs     = Delete y    : zipET xs ys zs
zipET (E.Match : xs)       (y:ys) (_:zs) = Id y        : zipET xs ys zs
zipET (E.Replace a b : xs) (_:ys) (_:zs) = Replace a b : zipET xs ys zs
zipET _                     []     []    = []
zipET _                     _      _     = error "invalid arguments"

printEdit' :: Bool -> E.EditTranscript Text -> [Text] -> [Text] -> IO ()
printEdit' isLine (op:ops) xs ys =
  case op of
    E.Insert -> do
      p $ green (toS (head ys))
      go ops xs (tail ys)
    E.Delete -> do
      p $ red (toS (head xs))
      go ops (tail xs) ys
    E.Match -> do
      p $ toS (head xs)
      go ops (tail xs) (tail ys)
    E.Replace x y -> do
      case (P.parse (cProg <* P.eof) "" x, P.parse (cProg <* P.eof) "" y) of
        (Right x', Right y') -> do
          let (_, t) = E.editDistance E.stdCost x' y'
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

printEdit :: E.EditTranscript Text -> [Text] -> [Text] -> IO ()
printEdit = printEdit' False
