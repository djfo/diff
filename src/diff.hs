module Main where

import System.Environment

import Edit
import Print

type F a = CostFunction a -> [a] -> [a] -> (Int, EditTranscript a)

dummy :: Eq a => F a
dummy cost xs ys = (0, greedyEditDistance cost xs ys)

diff :: F String -> FilePath -> FilePath -> IO ()
diff editDist path1 path2 =
  do
    lines1 <- lines <$> readFile path1
    lines2 <- lines <$> readFile path2
    let (c, t) = editDist cost lines1 lines2
    putStrLn $ "cost: " ++ show c
    printEdit t lines1 lines2
  where
    cost :: Op a -> Int
    cost op =
      case op of
        Replace _ _ -> 2
        Match -> 0
        _ -> 1

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
        Replace _ _ -> 0
        Match -> 0
        _ -> 1

main2 :: (String -> String -> IO ()) -> [String] -> IO ()
main2 f args =
  case args of
    [arg1, arg2] -> f arg1 arg2
    _ -> putStrLn "two arguments expected, too few given"

select :: String -> [String] -> IO ()
select cmd =
  case cmd of
    "diff" -> main2 (diff editDistance)
    "str" -> main2 (strDiff editDistance)
    "greedy" -> main2 (diff dummy)
    _ -> const (putStrLn $ "unknown command: " ++ cmd)

main :: IO ()
main = do
  args <- getArgs
  case args of
    cmd : cmdArgs -> select cmd cmdArgs
    _ -> putStrLn "no command given"
