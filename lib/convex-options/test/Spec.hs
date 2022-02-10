module Main where

import Test.Tasty (defaultMain)
import Convex.Options.Test(optionTests)

main :: IO ()
main = defaultMain optionTests
