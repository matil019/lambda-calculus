module Main where

import Test.Hspec

import qualified LambdaCalculus.DeBruijnSpec
import qualified LambdaCalculus.TermSpec

main :: IO ()
main = hspec $ do
  describe "DeBruijn" LambdaCalculus.DeBruijnSpec.spec
  describe "Term" LambdaCalculus.TermSpec.spec
