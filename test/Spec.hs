
module Main where

import Test.Hspec
import AppSpec (spec)

-- TODO: reset state before every run
main :: IO ()
main = hspec spec
