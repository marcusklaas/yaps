
module Main where

import           App
import           System.Environment
import           Safe

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, portStr] -> case readMay portStr of
      Just port -> run dir port
      _ -> putStrLn $ "Couldn't parse port " ++ portStr
    _ -> putStrLn "USAGE: yaps <dir> <port>"
