module Term (
    Term()
  , Color(..)
  , term
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , underline
  , blink
  ) where

import           Data.Maybe

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Enum, Show)

data Term
  = Term {
    tForeground :: Maybe Color
  , tBackground :: Maybe Color
  , tUnderline :: Bool
  , tBlink :: Bool
  }
  deriving (Eq, Ord, Show)

just :: Maybe a -> Maybe a -> Maybe a
just x Nothing  = x
just _ (Just x) = Just x

instance Monoid Term where
  mempty = Term Nothing Nothing False False
  mappend t s = s { tForeground = just (tForeground t) (tForeground s)
                  , tBackground = just (tBackground t) (tBackground s)
                  , tUnderline = tUnderline t || tUnderline s
                  , tBlink = tBlink t || tBlink s
                  }

color :: Color -> Term
color c = mempty { tForeground = Just c }

black, red, green, yellow, blue, magenta, cyan, white :: Term
black = color Black
red = color Red
green = color Green
yellow = color Yellow
blue = color Blue
magenta = color Magenta
cyan = color Cyan
white = color White

underline :: Term
underline = mempty { tUnderline = True }

blink :: Term
blink = mempty { tBlink = True }

term :: Term -> String -> String
term t s = escape ++ s ++ unescape
  where
    escape = concat (catMaybes [fg, u, bl])
    unescape = "\ESC[0m"
    fg = case tForeground t of
           Just c -> Just $ "\ESC[1;" ++ show (30 + fromEnum c) ++ "m"
           Nothing -> Nothing
    u = if tUnderline t then Just "\ESC[1;4m" else Nothing
    bl = if tBlink t then Just "\ESC[1;5m" else Nothing
