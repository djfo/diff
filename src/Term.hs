module Term (
    Term()
  , Color(..)
  , term
  , fg
  , bg
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , bold
  , underline
  , blink
  ) where

import           Data.List  (intercalate)
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
  , tBold :: Bool
  , tUnderline :: Bool
  , tBlink :: Bool
  }
  deriving (Eq, Ord, Show)

just :: Maybe a -> Maybe a -> Maybe a
just x Nothing  = x
just _ (Just x) = Just x

instance Monoid Term where
  mempty = Term Nothing Nothing False False False
  mappend t s = s { tForeground = just (tForeground t) (tForeground s)
                  , tBackground = just (tBackground t) (tBackground s)
                  , tBold = tBold t || tBold s
                  , tUnderline = tUnderline t || tUnderline s
                  , tBlink = tBlink t || tBlink s
                  }

fg :: Color -> Term
fg c = mempty { tForeground = Just c }

bg :: Color -> Term
bg c = mempty { tBackground = Just c }

black, red, green, yellow, blue, magenta, cyan, white :: Term
black = fg Black
red = fg Red
green = fg Green
yellow = fg Yellow
blue = fg Blue
magenta = fg Magenta
cyan = fg Cyan
white = fg White

bold :: Term
bold = mempty { tBold = True }

underline :: Term
underline = mempty { tUnderline = True }

blink :: Term
blink = mempty { tBlink = True }

term :: Term -> String -> String
term t s =
  if null attributes_
    then s
    else "\ESC[" ++ (intercalate ";" attributes_) ++ "m" ++ s ++ unescape
  where
    bool :: (Term -> Bool) -> Int -> Maybe String
    bool p c = if p t then Just (show c) else Nothing

    color :: (Term -> Maybe Color) -> Int -> Maybe String
    color f b =
      case f t of
        Just c -> Just $ show (b + fromEnum c)
        Nothing -> Nothing

    attributes_ = catMaybes [ color tForeground 30
                            , color tBackground 40
                            , bool tBold 1
                            , bool tUnderline 4
                            , bool tBlink 5
                            ]

unescape :: String
unescape = "\ESC[0m"
