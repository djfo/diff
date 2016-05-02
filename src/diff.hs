module Main where

import           Edit
import           Print

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment

type F a = CostFunction a -> [a] -> [a] -> (Int, EditTranscript a)

diff :: F Text -> FilePath -> FilePath -> IO ()
diff editDist path1 path2 =
  do
    lines1 <- T.lines <$> T.readFile path1
    lines2 <- T.lines <$> T.readFile path2
    let (c, t) = editDist cost lines1 lines2
    putStrLn $ "cost: " ++ show c
    printEdit t lines1 lines2
  where
    cost :: Op Text -> Int
    cost Match         = 0
    cost (Replace x y) = case x `T.commonPrefixes` y of
                           Just _  -> 0
                           Nothing -> 2
    cost _             = 1

test :: IO ()
test = main2 (diff editDistance) ["examples/game.c", "examples/game1.c"]

strDiff :: F Char -> String -> String -> IO ()
strDiff editDist s t =
  do
    let (c, et) = editDist cost s t
    putStrLn $ "cost: " ++ show c
    printEdit et s t
  where
    cost :: Op a -> Int
    cost op =
      case op of
        Replace _ _ -> 1
        Match       -> 0
        _           -> 1

main2 :: (String -> String -> IO ()) -> [String] -> IO ()
main2 f args =
  case args of
    [arg1, arg2] -> f arg1 arg2
    _            -> putStrLn "two arguments expected, too few given"

select :: String -> [String] -> IO ()
select cmd =
  case cmd of
    "diff" -> main2 (diff editDistance)
    "str"  -> main2 (strDiff editDistance)
    _      -> const (putStrLn $ "unknown command: " ++ cmd)

main :: IO ()
main = do
  args <- getArgs
  case args of
    cmd : cmdArgs -> select cmd cmdArgs
    _             -> putStrLn "no command given"
