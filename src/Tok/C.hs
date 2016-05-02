{-# LANGUAGE OverloadedStrings #-}

module Tok.C where

import           Control.Monad    (void)
import           Data.List        (sortOn)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text

-- TODO: allow more escape sequences
escape :: Parser String
escape = do
  void $ char '\\'
  c <- oneOf "bnt\""
  case c of
    'b' -> return "\b"
    'n' -> return "\n"
    't' -> return "\t"
    '"' -> return "\""
    _   -> unexpected $ "escape character: " ++ [c]

literalString :: Parser String
literalString = do
  void $ char '"'
  s <- many (escape <|> fmap (:[]) (noneOf "\""))
  void $ char '"'
  spaces
  return . concat $ s

punctuation :: Parser Text
punctuation = (T.pack . (: [])) <$> oneOf "#[](){}:><-+.*/%;,=&|^"

keywords :: [String]
keywords = [
    "break"
  , "case"
  , "char"
  , "do"
  , "else"
  , "for"
  , "if"
  , "int"
  , "return"
  , "struct"
  , "switch"
  , "void"
  , "while"
  ]

keyword :: Parser Text
keyword = foldr1 (<|>) $ map (fmap T.pack . string) (sortOn length keywords)

identifier :: Parser Text
identifier = do
  hd <- oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  tl <- many $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9']
  return (T.pack (hd : tl))

number :: Parser Text
number = T.pack <$> many1 (oneOf ['0'..'9'])

value :: Parser Text
value = (T.pack <$> literalString) <|> number <|> ((T.pack . (: [])) <$> anyChar)

cTok :: Parser Text
cTok = try keyword <|> identifier <|> punctuation <|> value <|> (fmap T.pack . many1 . oneOf $ " \t\n")

cProg :: Parser [Text]
cProg = many cTok
