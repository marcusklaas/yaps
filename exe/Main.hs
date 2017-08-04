
module Main where

import           App
import           System.Environment
import           Network.Wai.Handler.WarpTLS
import           Safe

main :: IO ()
main = do
  args <- getArgs
  case args of
    [certFilePath, keyFilePath, dir, portStr] -> case readMay portStr of
      Just port -> run (tlsSettings certFilePath keyFilePath) dir port
      _ -> putStrLn $ "Couldn't parse port " ++ portStr
    _ -> putStrLn "USAGE: yaps <tls cert file> <tls key file> <dir> <port>"
