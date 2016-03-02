module Main where

import System.Environment

import Edit
import Print

diff :: FilePath -> FilePath -> IO ()
diff path1 path2 =
  do
    lines1 <- lines <$> readFile path1
    lines2 <- lines <$> readFile path2
    let t = editDistance cost lines1 lines2
    printEdit t lines1 lines2
  where
    cost :: Op a -> Int
    cost op =
      case op of
        Replace _ _ -> 2
        Match -> 0
        _ -> 1

strDiff :: String -> String -> IO ()
strDiff s t =
  do
    let et = editDistance cost s t
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
    "diff" -> main2 diff
    "str" -> main2 strDiff
    _ -> const (putStrLn $ "unknown command: " ++ cmd)

main :: IO ()
main = do
  args <- getArgs
  case args of
    cmd : cmdArgs -> select cmd cmdArgs
    _ -> putStrLn "no command given"
