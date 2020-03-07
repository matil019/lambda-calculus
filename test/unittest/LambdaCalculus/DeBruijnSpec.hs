module LambdaCalculus.DeBruijnSpec where

import LambdaCalculus.DeBruijn
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  it "toDeBruijn . fromDeBruijn == id" pending

  -- an alternative to above until I define @instance Arbitrary DeBruijn.Term@
  prop "toDeBruijn . fromDeBruijn . toDeBruijn == toDeBruijn" $ \m ->
    (toDeBruijn . fromDeBruijn . toDeBruijn) m `shouldBe` toDeBruijn m
