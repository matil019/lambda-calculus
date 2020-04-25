module Main where

import Test.Hspec

import qualified LambdaCalculus.DeBruijnSpec
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.ParseSpec
import qualified LambdaCalculus.SimplyTyped.HindleyMilner.TypesSpec
import qualified LambdaCalculus.SimplyTyped.HindleyMilnerSpec
import qualified LambdaCalculus.TermSpec

main :: IO ()
main = hspec $ do
  describe "DeBruijn" LambdaCalculus.DeBruijnSpec.spec
  describe "Term" LambdaCalculus.TermSpec.spec
  describe "SimplyTyped.HindleyMilner" LambdaCalculus.SimplyTyped.HindleyMilnerSpec.spec
  describe "SimplyTyped.HindleyMilner.Parse" LambdaCalculus.SimplyTyped.HindleyMilner.ParseSpec.spec
  describe "SimplyTyped.HindleyMilner.Types" LambdaCalculus.SimplyTyped.HindleyMilner.TypesSpec.spec
