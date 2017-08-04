
module Main where

import Test.Hspec
import System.Directory
import AppSpec (spec)
import App (doubleHashFile, testFile)

setupTestDir :: String -> IO ()
setupTestDir dir = do
  createDirectoryIfMissing False dir
  copyFile ("test/" ++ testFile) (dir ++ testFile)
  copyFile ("test/" ++ doubleHashFile) (dir ++ doubleHashFile)

main :: IO ()
main = do
  setupTestDir "test/update/"
  setupTestDir "test/update-key/"
  hspec spec
