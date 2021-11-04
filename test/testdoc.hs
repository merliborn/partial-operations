module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["src/Control/Partial.hs"]