module Main where

import           Edit
import           Print

import           Control.Applicative ((<$>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           System.Environment

type F a = CostFunction a -> [a] -> [a] -> (Int, EditTranscript a)

diff :: F Text -> FilePath -> FilePath -> IO ()
diff editDist path1 path2 =
  do
    lines1 <- T.lines <$> T.readFile path1
    lines2 <- T.lines <$> T.readFile path2
    let (_, t) = editDist cost lines1 lines2
    printEdit t lines1 lines2
  where
    cost :: Op Text -> Int
    cost Match         = 0
    cost (Replace x y) = case x `T.commonPrefixes` y of
                           Just _  -> 0
                           Nothing -> 2
    cost _             = 1

main2 :: (String -> String -> IO ()) -> [String] -> IO ()
main2 f args =
  case args of
    [arg1, arg2] -> f arg1 arg2
    _            -> putStrLn "two arguments expected, too few given"

main :: IO ()
main = do
  args <- getArgs
  main2 (diff editDistance) args
