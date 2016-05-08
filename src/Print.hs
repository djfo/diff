{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Print (printEdit, zipET, chunks) where

import qualified Edit                as E
import           Term
import           Tok.C

import           Control.Applicative ((<*))
import           Data.List           (tails)
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

data DiffOp a
  = Id a
  | Insert a
  | Delete a
  | Replace a a
  deriving (Eq, Ord, Show)

idOp :: DiffOp a -> Bool
idOp (Id _) = True
idOp _      = False

zipET :: E.EditTranscript a -> [a] -> [a] -> [DiffOp a]
zipET (E.Insert : xs)      ys     (z:zs) = Insert z    : zipET xs ys zs
zipET (E.Delete : xs)      (y:ys) zs     = Delete y    : zipET xs ys zs
zipET (E.Match : xs)       (y:ys) (_:zs) = Id y        : zipET xs ys zs
zipET (E.Replace a b : xs) (_:ys) (_:zs) = Replace a b : zipET xs ys zs
zipET _                    []     []     = []
zipET _                    _      _      = error "invalid arguments"

chunks :: Int -> [DiffOp a] -> [DiffOp a] -> [[DiffOp a]]
chunks n (x:xs) ys =
  if idOp x then
    chunks n xs (take n (x:ys))
   else
    let (tmp, xs') = break (all idOp . take n) (tails xs)
        chunk      = reverse ys ++ [x] ++ take (length tmp + n) xs
    in chunk : chunks n (concat . take 1 $ xs') []
chunks _ [] _ = []

printEdit' :: Bool -> [DiffOp Text] -> IO ()
printEdit' isLine (op:ops) =
  case op of
    Id x -> do
      p "  " (toS x)
      go ops
    Insert x -> do
      p "+ " $ term green (toS x)
      go ops
    Delete x -> do
      p "- " $ term red (toS x)
      go ops
    Replace x y -> do
      case (P.parse (cProg <* P.eof) "" x, P.parse (cProg <* P.eof) "" y) of
        (Right x', Right y') -> do
          let (_, t) = E.editDistance E.stdCost x' y'
          putStr "~ "
          printEdit' True (zipET t x' y')
          putStrLn []
        _ -> do
          p "- " $ term red (toS x)
          p "+ " $ term green (toS y)
      printEdit' False ops
  where
    p x y = if isLine then putStr y else putStrLn (x ++ y)
    go = printEdit' isLine
printEdit' _ [] = return ()

printEdit :: E.EditTranscript Text -> [Text] -> [Text] -> IO ()
printEdit t xs ys = mapM_ (\e -> putStrLn "---" >> printEdit' False e) (chunks 3 (zipET t xs ys) [])
