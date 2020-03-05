module Main where

import Test.Hspec

import qualified LambdaCalculus.TermSpec

main :: IO ()
main = hspec $ do
  describe "Term" LambdaCalculus.TermSpec.spec
